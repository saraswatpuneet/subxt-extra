mod generator;
/// Substrate metadata to openapi spec module
mod types;
pub use self::generator::{
    generate_swagger_from_url,
    SwaggerGenerator,
};
