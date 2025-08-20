//! # rrhodium
//! *rrhodium* is a library focused in building urls for the modrinth API. Since it is a pain in
//! the ~ass~ to make them and specially the facets.
//!
//!
//! # Builder approach
//!
//! The approach for **bulding** the urls is a **BUILDER** pattern (who could tell?), which I
//! think it's very nice. Also it is implemented with *manual* type-state so you can't fuck it up
//! and build urls without specifying a search type.
//!
//! ```ignore
//! use rrhodium::*;
//!
//! fn buzz() {
//!     SearchBuilder::new().game_versions(vec!["1.19.2", "1.19.1"]).build_url()
//! }
//! ```
//! The above code **WON'T** compile, since it's mising the search type.
//!
//! # Facets approach
//!
//! For the facets this library approachs the problem by using two structs:
//! `FacetsConjunction` and `FacetsDisjunction`, since they must be added in
//! [CNF](<https://en.wikipedia.org/wiki/Conjunctive_normal_form>) way.
//!
//! ```no_run
//! use rrhodium::*;
//! fn foo() -> FacetsConjunction {
//! FacetsConjunction::new()
//!     .and(FacetsDisjunction::new().or(Facets::Categories("Technology".to_string())))
//!     .and(FacetsDisjunction::new().or(Facets::Categories("Magic".to_string())))
//!     .and(
//!         FacetsDisjunction::new()
//!             .or(Facets::Version("1.19".to_string()))
//!             .or(Facets::Version("1.19.1".to_string())),
//!     )
//! }
//! ```
//!
//! The above example means:
//!
//! I want my search to show (whatever you are searching) only results which categories are
//! "Technology" **AND** "Magic", **AND** the version must be 1.19 **OR** 1.19.1.
//!
//! Whatever is inside of every disjunction is an **OR** and everything that is inside a conjuntion
//! is an **AND**.
//!
//!
//! For now only the getters routes are provided, and I don't think I'll add the others
//! (create/delete/modify project)

use itertools::Itertools;
use std::{
    fmt::{Display, Formatter, Write},
    vec::IntoIter,
};

/// A type for representing that no search type is set.
type NoSearchType = ();

#[derive(Clone, Debug)]
pub enum HashingAlgo {
    Sha1,
    Sha512,
}

impl Display for HashingAlgo {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Sha1 => write!(f, "sha1"),
            Self::Sha512 => write!(f, "sha512"),
        }
    }
}

/// A list specifying the different kinds of requirements types.
/// - Optional
/// - Required
/// - Unsupported
#[derive(Debug, Copy, Clone)]
pub enum Requirement {
    Optional,
    Required,
    Unsupported,
}

impl Display for Requirement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Self::Optional => "optional",
            Self::Required => "required",
            Self::Unsupported => "unsupported",
        };
        write!(f, "{s}")
    }
}

/// A list specifying the different kinds of requests based on the API
/// routes.
#[derive(Debug, Clone)]
pub enum SearchType {
    /// Search projects
    ///
    /// /search
    ///
    /// It's recommended to build this variant through the [`QueryBuilder`] struct.
    Search {
        query: String,
        limit: Option<u32>,
        offset: Option<u32>,
    },

    // Get a project
    //
    /// /project
    Project { id: String },

    /// Get multiple projects
    ///
    /// /projects
    MultiProject { ids: Vec<&'static str> },

    /// Get all of a project's dependencies
    ///
    /// /project/{id|slug}/dependencies
    Dependencies { id: String },

    /// List project's versions
    ///
    /// /project/{id|slug}/version
    ///
    /// - loaders: The types of loaders to filter for.
    ProjectVersion {
        id: String,
        loaders: Option<Vec<String>>,
    },

    /// Get a version
    ///
    /// /version/{id}
    ///
    /// - id: The ID of the version
    Version { id: String },

    /// Get a version given a version number or ID
    ///
    /// Please note that, if the version number provided matches multiple versions, only the oldest
    /// matching version will be returned.
    ///
    /// Missing :(

    /// Get multiple versions
    ///
    /// /versions
    ///
    /// -ids The IDs of the versions
    ProjectVersions { ids: Vec<String> },

    /// Get version from hash
    ///
    /// /version_file/{hash}
    VersionFile { hash: String, algo: HashingAlgo },

    /// Latest version of a project from a hash, loader(s), and game version(s)
    ///
    /// /version_file/{hash}/update
    VersionFileUpdate { hash: String, algo: HashingAlgo },

    /// Get versions from hashes
    ///
    /// /version_files
    ///
    /// The hashes [**must** be in the body](https://docs.modrinth.com/api/operations/versionsfromhashes/) of the request
    VersionFiles,

    /// Latest versions of multiple project from hashes, loader(s), and game version(s)
    ///
    /// /version_files/update
    ///
    /// The hashes [**must** be in the body](https://docs.modrinth.com/api/operations/getlatestversionsfromhashes/) of the request
    VersionFilesUpdate,

    /// /tag/category
    Categories,

    /// /tag/loader
    Loaders,
}

impl Display for SearchType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            SearchType::Project { id } => write!(f, "project/{id}?"),
            SearchType::MultiProject { ids } => {
                let ids = ids
                    .iter()
                    .map(|id| format!("\"{id}\""))
                    .collect::<Vec<String>>()
                    .join(",");

                write!(f, "projects?ids=[{ids}]")
            }
            SearchType::Search { .. } => write!(f, "search"),
            SearchType::VersionFile { hash, .. } => write!(f, "version_file/{hash}"),
            SearchType::VersionFiles => write!(f, "version_files"),
            SearchType::Dependencies { id } => write!(f, "project/{id}/dependencies"),
            SearchType::ProjectVersion { id, .. } => write!(f, "project/{id}/version"),
            SearchType::ProjectVersions { ids } => {
                let ids = ids
                    .iter()
                    .map(|id| format!("\"{id}\""))
                    .collect::<Vec<String>>()
                    .join(",");

                write!(f, "versions?ids=[{ids}]")
            }
            SearchType::Version { id } => write!(f, "version/{id}"),
            SearchType::Categories => write!(f, "tag/category"),
            SearchType::Loaders => write!(f, "tag/loader"),
            SearchType::VersionFileUpdate { hash, .. } => write!(f, "version_file/{hash}/update"),
            SearchType::VersionFilesUpdate => write!(f, "/version_files/update"),
        }
    }
}

/// A builder for constructing [`SearchType::Search`] instances with optional parameters.
///
/// This builder provides a fluent interface for creating search queries with customizable
/// search terms, result limits, and pagination offsets. All parameters are optional,
/// allowing for flexible search construction.
///
/// # Examples
///
/// Basic search with just a query:
///
/// ```rust
/// use rrhodium::QueryBuilder;
///
/// let search = QueryBuilder::new()
///     .query("minecraft mods")
///     .build();
/// ```
/// # Default Behavior
///
/// - **Query**: Empty string (searches all content)
/// - **Limit**: Determined by the API default (typically 10-20 results)
/// - **Offset**: 0 (starts from the first result)
#[derive(Default)]
pub struct QueryBuilder {
    query: Option<String>,
    limit: Option<u32>,
    offset: Option<u32>,
}

impl QueryBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    // Sets the search query string.
    ///
    /// The query string is used to search for content matching the specified terms.
    /// Different APIs may support various query syntaxes (e.g., phrase matching,
    /// boolean operators, wildcards).
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use rrhodium::QueryBuilder;
    /// let search = QueryBuilder::new()
    ///     .query("sodium")
    ///     .build();
    /// ```
    ///
    /// With a dynamically generated query:
    ///
    /// ```rust
    /// # use rrhodium::QueryBuilder;
    /// let user_input = "adventure";
    /// let search = QueryBuilder::new()
    ///     .query(format!("{} mods", user_input))
    ///     .build();
    /// ```
    ///
    /// # Parameters
    ///
    /// * `query` - The search terms. Accepts anything that implements `Into<String>`
    ///
    /// # Returns
    ///
    /// The builder instance for method chaining
    pub fn query(mut self, query: impl Into<String>) -> Self {
        self.query = Some(query.into());
        self
    }

    /// Sets the maximum number of results to return.
    ///
    /// This parameter controls how many search results will be returned in a single
    /// response. Useful for controlling response size and implementing pagination.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use rrhodium::QueryBuilder;
    /// let search = QueryBuilder::new()
    ///     .limit(50)
    ///     .build();
    /// ```
    ///
    /// # Parameters
    ///
    /// * `limit` - Maximum number of results to return. Must be positive.
    ///
    /// # Returns
    ///
    /// The builder instance for method chaining
    ///
    /// # Notes
    ///
    /// The actual maximum limit may be constrained by the API server. Very large
    /// limits might be reduced to prevent performance issues.
    pub fn limit(mut self, limit: u32) -> Self {
        self.limit = Some(limit);
        self
    }

    /// Sets the number of results to skip (pagination offset).
    ///
    /// This parameter is used for pagination by skipping the specified number of results from the
    /// beginning. Combined with `limit`, it enables pagination through large result sets.
    ///
    /// # Examples
    ///
    /// Getting the second page of results (assuming 20 results per page):
    ///
    /// ```rust
    /// # use rrhodium::QueryBuilder;
    /// let page_2 = QueryBuilder::new()
    ///     .query("popular mods")
    ///     .limit(20)
    ///     .offset(20)  // Skip first 20 results
    ///     .build();
    /// ```
    ///
    /// # Parameters
    ///
    /// * `offset` - Number of results to skip from the beginning
    ///
    /// # Returns
    ///
    /// The builder instance for method chaining
    pub fn offset(mut self, offset: u32) -> Self {
        self.offset = Some(offset);
        self
    }

    /// Builds the final [`SearchType::Search`] instance.
    ///
    /// Consumes the builder and creates a `SearchType::Search` with the configured
    /// parameters. Any unset optional parameters will use their default values.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use rrhodium::{SearchBuilder, QueryBuilder};
    /// let search = QueryBuilder::new()
    ///     .query("minecraft")
    ///     .limit(25)
    ///     .offset(10)
    ///     .build();
    ///
    ///
    /// assert_eq!(
    ///     "https://api.modrinth.com/v2/search?query=\"minecraft\"&limit=25&offset=10",
    ///     SearchBuilder::new().search_type(search).build_url()
    /// );
    /// ```
    ///
    /// # Returns
    ///
    /// A configured `SearchType::Search` instance ready for use
    pub fn build(self) -> SearchType {
        SearchType::Search {
            query: self.query.unwrap_or_default(), // Empty string if no query provided
            limit: self.limit,
            offset: self.offset,
        }
    }
}

/// A builder for building the URL with the indicated parameters
/// This struct works with `TypeState` Programming so the [`SearchBuilder::build_url()`] method
/// can't be called unless `search_type` is set.
///
/// The `facets` field works as a conjunction (AND) of disjunctions (OR):
#[derive(Default)]
pub struct SearchBuilder<T = NoSearchType> {
    search_type: T,
    facets: Option<FacetsConjunction>,
    game_versions: Vec<String>,
}

impl SearchBuilder<NoSearchType> {
    #[must_use]
    pub fn new() -> SearchBuilder<NoSearchType> {
        SearchBuilder::default()
    }
}

impl<T> SearchBuilder<T> {
    /// Sets the facets.
    #[must_use]
    pub fn facets(mut self, facets: impl Into<FacetsConjunction>) -> Self {
        self.facets = Some(facets.into());
        self
    }

    /// Sets the game versions filter for the project version search.
    ///
    ///
    /// # Parameters
    ///
    /// - `versions`: A type that can be iterated and its items implements `ToString`.
    ///
    /// # Returns
    ///
    /// Returns an updated instance of the `SearchBuilder` with the specified
    /// game versions filter applied.
    ///
    /// # Example
    ///
    /// ```rust no_run
    /// use rrhodium::*;
    /// # fn foo() {
    /// let builder = SearchBuilder::new()
    ///     .search_type(SearchType::ProjectVersion {id: "example_id".to_owned(), loaders:
    ///     Some(vec!["fabric".to_string()])})
    ///     .game_versions(["1.16.5", "1.17.1"])
    ///     .build_url();
    /// # }
    /// ```
    ///
    /// # Restrictions
    ///
    /// This method is only available when the search type is `ProjectVersion`.
    /// Attempting to call this method for other search types will do nothing.
    ///
    /// # Panics
    ///
    /// This method does not panic.
    ///
    /// # Notes
    ///
    /// - The `game_versions` parameter allows you to filter results to only
    ///   include project versions compatible with the specified game versions.
    /// - Ensure that you are using this method within the `ProjectVersion`
    ///   search context, as it is specifically designed for filtering versions
    ///   based on game compatibility.
    #[must_use]
    pub fn game_versions<E>(mut self, versions: impl IntoIterator<Item = E>) -> Self
    where
        E: ToString,
    {
        self.game_versions = versions.into_iter().map(|x| x.to_string()).collect_vec();
        self
    }

    /// Adds a single game version to the game versions filter for the project
    /// version search.
    ///
    /// This method allows you to append a game version to the existing filter
    /// criteria in the `SearchBuilder`. The version is added to the vector
    /// of game versions that will be used to filter the results of the
    /// search.
    ///
    /// # Parameters
    ///
    /// - `version`: Whatever type that can be transformed into a `String`.
    ///
    /// # Returns
    ///
    /// Returns an updated instance of the `SearchBuilder` with the specified
    /// game version added to the filter criteria.
    ///
    /// # Example
    ///
    /// ```no_run
    /// use rrhodium::{SearchType, SearchBuilder};
    /// let builder = SearchBuilder::new()
    ///     .search_type(SearchType::ProjectVersion {id: "example_id".to_string(), loaders: Some(vec!["fabric".to_string()])})
    ///     .add_game_version("1.16.5")
    ///     .add_game_version("1.17.1")
    ///     .build_url();
    /// ```
    ///
    /// # Restrictions
    ///
    /// This method only has an effect when the search type is `ProjectVersion`.
    /// If used with a different search type, it will have no impact on the
    /// search builder and will silently ignore the call.
    ///
    /// # Panics
    ///
    /// This method does not panic.
    ///
    /// # Errors
    ///
    /// This method does not return errors.
    ///
    /// # Notes
    /// - Ensure that this method is called within the `ProjectVersion` search
    ///   context, as it is specifically designed for filtering project versions
    ///   based on game compatibility.
    /// - If used outside the `ProjectVersion` context, the method will not
    ///   modify the builder and will effectively do nothing.
    #[must_use]
    pub fn add_game_version(mut self, version: impl ToString) -> Self {
        self.game_versions.push(version.to_string());
        self
    }

    pub fn search_type(self, search_type: SearchType) -> SearchBuilder<SearchType> {
        SearchBuilder {
            search_type,
            facets: self.facets,
            game_versions: self.game_versions,
        }
    }
}

impl SearchBuilder<SearchType> {
    /// Generates the URL based on the `SearchBuilder` object.
    ///
    /// This method constructs a URL for the Modrinth API using various
    /// parameters from the `SearchBuilder` struct. It supports multiple
    /// search types and query parameters, and it constructs the URL
    /// accordingly.
    ///
    /// # Returns
    ///
    /// A `String` representing the constructed URL.
    ///
    /// # Examples
    ///
    /// ```rust no_run
    /// use rrhodium::*;
    /// # fn foo() {
    ///     let search_builder: String = SearchBuilder::new()
    ///     .search_type(QueryBuilder::new().limit(10).offset(5).build())
    ///     .build_url();
    /// assert_eq!("https://api.modrinth.com/v2/search?limit=10&offset=5", &search_builder);
    /// # }
    /// ```
    #[must_use]
    pub fn build_url(self) -> String {
        // Don't you like my unwraps ? I don't like you either dw ;)
        let mut url: String = "https://api.modrinth.com/v2/".to_string();
        write!(url, "{}", self.search_type).unwrap();

        let mut is_first = true;
        let add_param = |url: &mut String, is_first: &mut bool| {
            if *is_first {
                url.push('?');
                *is_first = false;
            } else {
                url.push('&');
            }
        };

        match &self.search_type {
            // If SearchType is Categories or Loaders there is no need to apply
            // queries/facets...
            SearchType::Categories | SearchType::Loaders => {
                return url;
            }
            SearchType::Search {
                query,
                limit,
                offset,
            } => {
                if !query.is_empty() {
                    add_param(&mut url, &mut is_first);
                    write!(url, "query=\"{query}\"").unwrap();
                }
                if let Some(limit) = limit {
                    add_param(&mut url, &mut is_first);
                    write!(url, "limit={limit}").unwrap();
                }
                if let Some(offset) = offset {
                    add_param(&mut url, &mut is_first);
                    write!(url, "offset={offset}").unwrap();
                }
                if self.facets.as_ref().is_some_and(|f| !f.is_empty()) {
                    add_param(&mut url, &mut is_first);
                    self.add_facets(&mut url);
                }
            }
            SearchType::ProjectVersion { loaders, .. } => {
                let versions = self
                    .game_versions
                    .iter()
                    .map(|v| format!("\"{v}\""))
                    .join(",");
                add_param(&mut url, &mut is_first);
                write!(url, "game_versions=[{versions}]").unwrap();

                if let Some(loaders) = loaders {
                    let loaders = loaders.iter().map(|v| format!("\"{v}\"")).join(",");
                    add_param(&mut url, &mut is_first);
                    write!(url, "loaders=[{loaders}]").unwrap();
                }
            }
            SearchType::VersionFile { algo, .. } | SearchType::VersionFileUpdate { algo, .. } => {
                add_param(&mut url, &mut is_first);
                write!(url, "algorithm={algo}").unwrap();
            }
            _ => {}
        }

        url
    }

    fn add_facets(&self, url: &mut String) {
        if let Some(ref facets) = self.facets {
            let facets_str = facets
                .iter()
                .map(|conjunction| format!("[{}]", conjunction.facets.iter().join(",")))
                .join(",");

            write!(url, "facets=[{facets_str}]").unwrap();
        }
    }
}

/// A collection of disjunctions that represents a logical AND operation (conjunction).
///
/// This struct combines multiple [`FacetsDisjunction`]s where **all** of them must be
/// satisfied for a search result to match. Each disjunction within the conjunction
/// represents an OR operation, creating complex logical expressions.
///
/// The logical structure follows the pattern: `(A ∨ B ∨ C) ∧ (D ∨ E) ∧ (F)`
/// where ∨ represents OR and ∧ represents AND.
///
/// ```rust
/// use rrhodium::{FacetsConjunction, FacetsDisjunction, Facets};
///
/// let version_filter = FacetsDisjunction::new()
///     .or(Facets::Version("1.19".to_string()))
///     .or(Facets::Version("1.19.1".to_string()))
///     .or(Facets::Version("1.19.2".to_string()));
///
/// let category_filter = FacetsDisjunction::new()
///     .or(Facets::Categories("technology".to_string()))
///     .or(Facets::Categories("magic".to_string()));
///
/// let search_filter = FacetsConjunction::new()
///     .and(version_filter)
///     .and(category_filter);
/// ```
///
/// # Search Logic
///
/// In search queries, this conjunction ensures that:
/// - **ALL** disjunctions must be satisfied
/// - Within each disjunction, **ANY ONE** of the facets can match
/// - Results must match every constraint group simultaneously
///
#[derive(Debug, Clone, Default)]
pub struct FacetsConjunction {
    disjunctions: Vec<FacetsDisjunction>,
}

impl FacetsConjunction {
    #[must_use]
    pub fn new() -> Self {
        Self {
            disjunctions: vec![],
        }
    }

    // Adds a disjunction to this conjunction using a builder pattern.
    ///
    /// This method consumes `self` and returns a new instance with the disjunction added,
    /// allowing for method chaining. The added disjunction becomes an additional
    /// requirement that must be satisfied along with all existing disjunctions.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use rrhodium::{FacetsConjunction, FacetsDisjunction, Facets};
    /// let version_filter = FacetsDisjunction::new()
    ///     .or(Facets::Version("1.19".to_string()));
    ///
    /// let category_filter = FacetsDisjunction::new()
    ///     .or(Facets::Categories("technology".to_string()));
    ///
    /// let conjunction = FacetsConjunction::new()
    ///     .and(version_filter)
    ///     .and(category_filter);
    /// ```
    ///
    /// # Parameters
    ///
    /// * `disj` - The disjunction to add to this conjunction
    ///
    /// # Returns
    ///
    /// A new `FacetsConjunction` instance with the added disjunction
    #[must_use]
    pub fn and(mut self, disj: FacetsDisjunction) -> Self {
        self.disjunctions.push(disj);
        self
    }

    /// Returns `true` if this conjunction contains no disjunctions.
    ///
    /// An empty conjunction typically matches all results since there are
    /// no constraints to satisfy.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use rrhodium::{FacetsConjunction, FacetsDisjunction, Facets};
    /// let empty = FacetsConjunction::new();
    /// assert!(empty.is_empty());
    ///
    /// let non_empty = FacetsConjunction::new()
    ///     .and(FacetsDisjunction::new().or(Facets::Version("1.19".to_string())));
    /// assert!(!non_empty.is_empty());
    /// ```
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.disjunctions.is_empty()
    }

    /// Returns an iterator over the disjunctions in this conjunction.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use rrhodium::{FacetsConjunction, FacetsDisjunction, Facets};
    /// let conjunction = FacetsConjunction::new()
    ///     .and(FacetsDisjunction::new().or(Facets::Version("1.19".to_string())))
    ///     .and(FacetsDisjunction::new().or(Facets::Categories("tech".to_string())));
    ///
    /// for disjunction in conjunction.iter() {
    ///     println!("Disjunction with {} facets", disjunction);
    /// }
    /// ```
    pub fn iter(&self) -> std::slice::Iter<'_, FacetsDisjunction> {
        self.disjunctions.as_slice().iter()
    }
}

impl<'a> IntoIterator for &'a FacetsConjunction {
    type Item = &'a FacetsDisjunction;
    type IntoIter = std::slice::Iter<'a, FacetsDisjunction>;
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl IntoIterator for FacetsConjunction {
    type Item = FacetsDisjunction;
    type IntoIter = IntoIter<FacetsDisjunction>;
    fn into_iter(self) -> Self::IntoIter {
        self.disjunctions.into_iter()
    }
}

impl std::convert::From<Vec<FacetsDisjunction>> for FacetsConjunction {
    fn from(value: Vec<FacetsDisjunction>) -> Self {
        Self {
            disjunctions: value,
        }
    }
}

///// A collection of facets that represents a logical OR operation (disjunction).
///
/// This struct is used to group multiple [`Facets`] together where any one of them
/// can match the search criteria. When used in a search query, at least one of
/// the contained facets must be satisfied for a result to match.
///
/// # Search Logic
///
/// In the context of search queries, this disjunction works as follows:
/// - **1.19.0 OR 1.19.1**: Returns projects compatible with either version
/// - **Technology OR Adventure**: Returns projects in either category
/// - **Forge OR Fabric**: Returns projects supporting either mod loader
#[derive(Debug, Clone, Default)]
pub struct FacetsDisjunction {
    facets: Vec<Facets>,
}

impl FacetsDisjunction {
    #[must_use]
    pub fn new() -> Self {
        Self { facets: vec![] }
    }

    /// Adds a facet to this disjunction by mutating the current instance.
    ///
    /// This method modifies the disjunction in place. For a builder-style
    /// approach that doesn't require mutability, see [`or`](Self::or).
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use rrhodium::{FacetsDisjunction, Facets};
    /// let mut disjunction = FacetsDisjunction::new();
    /// disjunction.push(Facets::Version("1.19.0".to_string()));
    /// disjunction.push(Facets::Version("1.19.1".to_string()));
    /// ```
    ///
    pub fn push(&mut self, facet: Facets) {
        self.facets.push(facet);
    }

    /// Adds a facet to this disjunction using a builder pattern.
    ///
    /// This method consumes `self` and returns a new instance with the facet added,
    /// allowing for method chaining without requiring mutability.
    ///
    /// # Examples
    ///
    /// ```rust
    /// # use rrhodium::{FacetsDisjunction, Facets};
    /// let disjunction = FacetsDisjunction::new()
    ///     .or(Facets::Categories("technology".to_string()))
    ///     .or(Facets::Categories("adventure".to_string()))
    ///     .or(Facets::Categories("utility".to_string()));
    /// ```
    /// # Returns
    ///
    /// A new `FacetsDisjunction` instance with the added facet.
    #[must_use]
    pub fn or(mut self, facet: Facets) -> Self {
        self.push(facet);
        self
    }
}

impl Display for FacetsDisjunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = self.facets.iter().map(|f| f.to_string()).join("v");
        write!(f, "{s}")
    }
}

/// A list specifying the different kinds of facets/filters that can be applied
/// to queries.
#[derive(Debug, Clone)]
pub enum Facets {
    ProjectType(String),
    Categories(String),
    Version(String),
    ClientSide(Requirement),
    ServerSide(Requirement),
    OpenSource,
}

impl Display for Facets {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Facets::ProjectType(t) => format!("\"project_type:{t}\""),
            Facets::Categories(c) => format!("\"categories:{c}\""),
            Facets::Version(v) => format!("\"versions:{v}\""),
            Facets::ClientSide(r) => format!("\"client_side:{r}\""),
            Facets::ServerSide(r) => format!("\"server_side:{r}\""),
            Facets::OpenSource => todo!(),
        };
        f.write_str(s.as_str())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn search_builder() {
        let search_type = QueryBuilder::new().limit(5).offset(10).build();

        let url = SearchBuilder::new().search_type(search_type).build_url();

        assert_eq!("https://api.modrinth.com/v2/search?limit=5&offset=10", url)
    }

    #[test]
    pub fn search_builder_facets() {
        let versions_facets = FacetsDisjunction::new()
            .or(Facets::Version("1.21".to_string()))
            .or(Facets::Version("1.20".to_string()));

        let url = SearchBuilder::new()
            .facets(vec![versions_facets])
            .search_type(QueryBuilder::new().offset(10).limit(5).build())
            .build_url();
        assert_eq!(
            "https://api.modrinth.com/v2/search?limit=5&offset=10&facets=[\
                [\"versions:1.21\",\"versions:1.20\"]\
            ]",
            url
        );
    }

    #[test]
    pub fn search_builder_facets_disjunction() {
        let mut versions_facets = FacetsDisjunction::new();
        versions_facets.push(Facets::Version("1.21".to_string()));
        versions_facets.push(Facets::Version("1.20".to_string()));

        let mut type_facets = FacetsDisjunction::new();
        type_facets.push(Facets::ProjectType("modpack".to_string()));

        let url = SearchBuilder::new()
            .facets(vec![versions_facets, type_facets])
            .search_type(QueryBuilder::new().offset(10).limit(5).build())
            .build_url();

        assert_eq!(
            "https://api.modrinth.com/v2/search?limit=5&offset=10&facets=[\
                [\"versions:1.21\",\"versions:1.20\"],\
                [\"project_type:modpack\"]\
            ]",
            url
        );
    }

    #[test]
    pub fn search_builder_facets_disjunction_builder() {
        let mut type_facets = FacetsDisjunction::new();
        type_facets.push(Facets::ProjectType("modpack".to_string()));

        let facets = FacetsConjunction::new()
            .and(
                FacetsDisjunction::new()
                    .or(Facets::Version("1.19".to_string()))
                    .or(Facets::Version("1.22".to_string())),
            )
            .and(FacetsDisjunction::new().or(Facets::ProjectType("modpack".to_string())))
            .and(
                FacetsDisjunction::new()
                    .or(Facets::Categories("technology".to_string()))
                    .or(Facets::Categories("adventure".to_string())),
            );

        let url = SearchBuilder::new()
            .facets(facets)
            .search_type(QueryBuilder::new().offset(10).limit(5).build())
            .build_url();

        assert_eq!(
            "https://api.modrinth.com/v2/search?limit=5&offset=10&facets=[\
                [\"versions:1.19\",\"versions:1.22\"],\
                [\"project_type:modpack\"],\
                [\"categories:technology\",\"categories:adventure\"]\
            ]",
            url
        );
    }

    #[test]
    pub fn search_builder_projects() {
        let url = SearchBuilder::new()
            .search_type(SearchType::MultiProject {
                ids: vec!["AAA", "BBB"],
            })
            .build_url();

        assert_eq!(
            "https://api.modrinth.com/v2/projects?ids=[\"AAA\",\"BBB\"]",
            url
        )
    }

    #[test]
    pub fn search_builder_project_versions() {
        let url = SearchBuilder::new()
            .search_type(SearchType::ProjectVersion {
                id: "Jw3Wx1KR".to_string(),
                loaders: None,
            })
            .game_versions(["1.18", "1.18.2"])
            .build_url();

        assert_eq!(
            "https://api.modrinth.com/v2/project/Jw3Wx1KR/version?game_versions=[\"1.18\",\"1.18.2\"]",
            url
        );
    }

    #[test]
    pub fn search_builder_project_versions_one_loader() {
        let url = SearchBuilder::new()
            .search_type(SearchType::ProjectVersion {
                id: "Jw3Wx1KR".to_string(),
                loaders: Some(vec!["fabric".to_string()]),
            })
            .game_versions(["1.18", "1.18.2"])
            .build_url();

        assert_eq!(
            "https://api.modrinth.com/v2/project/Jw3Wx1KR/version?game_versions=[\"1.18\",\"1.18.2\"]&loaders=[\"fabric\"]",
            url
        );
    }

    #[test]
    pub fn search_builder_project_versions_two_loaders() {
        let url = SearchBuilder::new()
            .search_type(SearchType::ProjectVersion {
                id: "Jw3Wx1KR".to_string(),
                loaders: Some(vec!["fabric".to_string(), "quilt".to_string()]),
            })
            .game_versions(["1.18", "1.18.2"])
            .build_url();

        assert_eq!(
            "https://api.modrinth.com/v2/project/Jw3Wx1KR/version?game_versions=[\"1.18\",\"1.18.2\"]&loaders=[\"fabric\",\"quilt\"]",
            url
        );
    }
}
