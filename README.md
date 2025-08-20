# Rrrhodium

[![Crates.io](https://img.shields.io/crates/v/rrhodium)](https://crates.io/crates/rrhodium)
[![License](https://img.shields.io/crates/l/rrhodium)](LICENSE)
[![Documentation](https://img.shields.io/docsrs/rrhodium)](https://docs.rs/rrhodium)

A Rust library for building URLs for the Modrinth API with type-safe builder
patterns and proper facet handling.

## Features

- **Type-safe URL building**: Compile-time guarantees that your API requests are properly constructed
- **Builder pattern**: Fluent interface for constructing complex API queries
- **Facet system**: Proper handling of Modrinth's CNF (Conjunctive Normal Form) facet requirements

## Installation

Add rrhodium to your `Cargo.toml`:

```toml
[dependencies]
rrhodium = "0.1"
```

# Usage
## Basic Search Building

```rust
use rrhodium::*;

// Build a search URL for projects
let url = SearchBuilder::new()
    .search_type(SearchType::Projects)
    .game_versions(vec!["1.19.2", "1.19.1"])
    .build_url();
```

## Facet Construcction


```rust
use rrhodium::*;

FacetsConjunction::new()
    .and(
        FacetsDisjunction::new()
        .or(Facets::Categories("Technology".to_string()))
    )
    .and(
        FacetsDisjunction::new()
        .or(Facets::Categories("Magic".to_string()))
    )
    .and(
        FacetsDisjunction::new()
        .or(Facets::Version("1.19".to_string()))
        .or(Facets::Version("1.19.1".to_string())),
    )
```

# API Coverage

Currently, rhodium supports Modrinth's GET endpoints for:

- Project search and retrieval
- Facet-based filtering
- Various query parameters

Note: Write operations (create/delete/modify projects) are not currently
supported and may not be added in the future.

