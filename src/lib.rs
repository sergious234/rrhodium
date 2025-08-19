//! # rhodium
//! *rhodium* is a library focused in building urls for the modrinth API. Since it is a pain in
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
//! use rhodium::*;
//!
//! fn buzz() {
//!     SearchBuilder::new().limit(10).offset(20).build_url()
//! }
//! ```
//! The above code **WON'T** compile, since it's mising the search type.
//!
//! # Facets approach
//!
//! For the facets this library approachs the problem by using two structs:
//! FacetsConjunction and FacetsDisjunction, since they must be added in
//! [CNF](<https://en.wikipedia.org/wiki/Conjunctive_normal_form>) way.
//!
//! ```no_run
//! use rhodium::*;
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

use itertools::Itertools;
use std::{
    fmt::{Display, Formatter, Write},
    vec::IntoIter,
};

/// A type for representing that no search type is set.
type NoSearchType = ();

/// A list specifying the different kinds of requests based on the API
/// routes.
#[derive(Debug, Clone)]
pub enum SearchType {
    /// /search
    Search,
    /// /project
    Project { id: String },
    /// /project/{id|slug}/version
    ProjectVersion { id: String },
    /// /projects
    MultiProject { ids: Vec<&'static str> },
    /// /version_file/{hash}
    VersionFile { hash: String },
    /// /version_files
    VersionFiles,
    /// /project/{id|slug}/dependencies
    Dependencies { id: String },
    /// /tag/category
    Categories,
    /// /tag/loader
    Loaders,
}

/// A builder for building the URL with the indicated parameters
/// This struct works with TypeState Programming so the `build_url()` method
/// can't be called unless search_type is set.
///
/// The `facets` field works as a conjunction (AND) of disjunctions (OR):
#[derive(Default)]
pub struct SearchBuilder<T> {
    search_type: T,
    facets: Option<FacetsConjunction>,
    query: Option<String>,
    limit: Option<u32>,
    offset: Option<u32>,
    game_versions: Vec<String>,
}

impl SearchBuilder<NoSearchType> {
    pub fn new() -> SearchBuilder<NoSearchType> {
        SearchBuilder {
            search_type: (),
            facets: None,
            limit: None,
            offset: None,
            query: None,
            game_versions: vec![],
        }
    }
}

impl<T> SearchBuilder<T> {
    /// Sets the facets.
    pub fn facets(mut self, facets: impl Into<FacetsConjunction>) -> Self {
        self.facets = Some(facets.into());
        self
    }

    /// Sets the limit.
    pub fn limit(mut self, limit: u32) -> Self {
        self.limit = Some(limit);
        self
    }

    /// Sets the offset.
    pub fn offset(mut self, offset: u32) -> Self {
        self.offset = Some(offset);
        self
    }

    /// Sets the game versions filter for the project version search.
    ///
    ///
    /// # Parameters
    ///
    /// - `versions`: A type that can be iterated and its items implements ToString.
    ///
    /// # Returns
    ///
    /// Returns an updated instance of the `SearchBuilder` with the specified
    /// game versions filter applied.
    ///
    /// # Example
    ///
    /// ```rust no_run
    /// use rhodium::*;
    /// let builder = SearchBuilder::new()
    ///     .search_type(SearchType::ProjectVersion {id: "example_id".to_owned()})
    ///     .game_versions(["1.16.5", "1.17.1"])
    ///     .build_url();
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
    /// use rhodium::{SearchType, SearchBuilder};
    /// let builder = SearchBuilder::new()
    ///     .search_type(SearchType::ProjectVersion {id: "example_id".to_string()})
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
    pub fn add_game_version(mut self, version: impl ToString) -> Self {
        self.game_versions.push(version.to_string());
        self
    }

    pub fn search_type(self, search_type: SearchType) -> SearchBuilder<SearchType> {
        SearchBuilder {
            search_type,
            query: self.query,
            facets: self.facets,
            offset: self.offset,
            limit: self.limit,
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
    /// use rhodium::*;
    /// # fn foo() {
    ///     let search_builder: String = SearchBuilder::new()
    ///     .limit(10)
    ///     .offset(5)
    ///     .search_type(SearchType::Search)
    ///     .build_url();
    /// assert_eq!("https://api.modrinth.com/v2/search?limit=10&offset=5", &search_builder);
    /// # }
    /// ```
    pub fn build_url(self) -> String {
        // Don't you like my unwraps ? I don't like you either dw ;)
        let mut url: String = "https://api.modrinth.com/v2/".to_string();

        let component = match &self.search_type {
            SearchType::Project { id } => &format!("project/{id}?"),
            SearchType::MultiProject { ids } => {
                let ids = ids
                    .iter()
                    .map(|id| format!("\"{id}\""))
                    .collect::<Vec<String>>()
                    .join(",");

                &format!("projects?ids=[{ids}]")
            }
            SearchType::Search => "search",
            SearchType::VersionFile { hash } => &format!("version_file/{hash}"),
            SearchType::VersionFiles => "version_files",
            SearchType::Dependencies { .. } => todo!("Not implemented yet"),
            SearchType::ProjectVersion { id } => &format!("project/{id}/version"),

            // If SearchType is Categories or Loaders there is no need to apply
            // queries/facets...
            SearchType::Categories => {
                write!(&mut url, "tag/category").unwrap();
                return url;
            }
            SearchType::Loaders => {
                write!(&mut url, "tag/loader").unwrap();
                return url;
            }
        };
        write!(url, "{component}").unwrap();

        if !self.game_versions.is_empty()
            && matches!(self.search_type, SearchType::ProjectVersion { .. })
        {
            let versions = self
                .game_versions
                .iter()
                .map(|v| format!("\"{v}\""))
                .join(",");
            return format!("{url}?game_versions=[{versions}]");
        }

        let mut is_first = true;
        let add_param = |url: &mut String, is_first: &mut bool| {
            if *is_first {
                url.push('?');
                *is_first = false;
            } else {
                url.push('&');
            }
        };

        if let Some(query) = self.query {
            add_param(&mut url, &mut is_first);
            write!(url, "query={query}").unwrap();
        }

        if let Some(limit) = self.limit {
            add_param(&mut url, &mut is_first);
            write!(url, "limit={limit}").unwrap();
        }

        if let Some(offset) = self.offset {
            add_param(&mut url, &mut is_first);
            write!(url, "offset={offset}").unwrap();
        }

        if let Some(facets) = self.facets
            && !facets.is_empty()
        {
            add_param(&mut url, &mut is_first);
            let facets_str = facets
                .iter()
                .map(|conjunction| format!("[{}]", conjunction.facets.iter().join(",")))
                .join(",");

            url.push_str(&format!("facets=[{facets_str}]"));
        }
        url
    }
}

/// This struct represent a Conjuntion (AND) of disjunctions (OR).
/// (A v B v C) ^ (A v B)
/// (1.19 **OR** 1.19.1 **OR** 1.19.2) **AND** (Technology **OR** Magic)
#[derive(Debug, Clone, Default)]
pub struct FacetsConjunction {
    disjunctions: Vec<FacetsDisjunction>,
}

impl FacetsConjunction {
    pub fn new() -> Self {
        Self {
            disjunctions: vec![],
        }
    }

    pub fn and(mut self, disj: FacetsDisjunction) -> Self {
        self.disjunctions.push(disj);
        self
    }

    pub const fn is_empty(&self) -> bool {
        self.disjunctions.is_empty()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, FacetsDisjunction> {
        self.into_iter()
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

/// This struct represent a disjunction (OR) of facets.
///
/// 1.19.0 **OR** 1.19.1
/// Technology **OR** Adventure
#[derive(Debug, Clone, Default)]
pub struct FacetsDisjunction {
    facets: Vec<Facets>,
}

impl FacetsDisjunction {
    pub fn new() -> Self {
        Self { facets: vec![] }
    }

    pub fn push(&mut self, facet: Facets) {
        self.facets.push(facet)
    }

    pub fn or(mut self, facet: Facets) -> Self {
        self.facets.push(facet);
        self
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn search_builder() {
        let url = SearchBuilder::new()
            .offset(10)
            .limit(5)
            .search_type(SearchType::Search)
            .build_url();

        assert_eq!("https://api.modrinth.com/v2/search?limit=5&offset=10", url)
    }

    #[test]
    pub fn search_builder_facets() {
        let versions_facets = FacetsDisjunction::new()
            .or(Facets::Version("1.21".to_string()))
            .or(Facets::Version("1.20".to_string()));

        let url = SearchBuilder::new()
            .offset(10)
            .limit(5)
            .facets(vec![versions_facets])
            .search_type(SearchType::Search)
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
            .offset(10)
            .limit(5)
            .facets(vec![versions_facets, type_facets])
            .search_type(SearchType::Search)
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
    pub fn search_builder_facets_disjunction2() {
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
            .offset(10)
            .limit(5)
            .facets(facets)
            .search_type(SearchType::Search)
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
            })
            .game_versions(["1.18", "1.18.2"])
            .build_url();

        assert_eq!(
            "https://api.modrinth.com/v2/project/Jw3Wx1KR/version?game_versions=[\"1.18\",\"1.18.2\"]",
            url
        );
    }

    pub fn build_not_allowed() {}
}
