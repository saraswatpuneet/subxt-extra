#[allow(unused_imports)]
use subxt_metadata::get_metadata_per_pallet_hash;

use crate::CratePath;
use codec::Decode;
use frame_metadata::{
    v14::RuntimeMetadataV14,
    RuntimeMetadata,
    RuntimeMetadataPrefixed,
};
use heck::ToSnakeCase as _;
use proc_macro2::TokenStream as TokenStream2;
use proc_macro_error::abort_call_site;
use quote::{
    format_ident,
    quote,
};
use std::{
    collections::HashMap,
    fs,
    io::Read,
    path,
    string::ToString,
};
use syn::parse_quote;

pub fn generate_swagger_from_url(
    item_mod: syn::ItemMod,
    bytes: &[u8],
    crate_path: CratePath,
) -> TokenStream2 {
    let metadata = frame_metadata::RuntimeMetadataPrefixed::decode(&mut &bytes[..])
        .unwrap_or_else(|e| abort_call_site!("Failed to decode metadata: {}", e));

    let swagger_generator = SwaggerGenerator::new(metadata);
    swagger_generator.generate_swagger(item_mod, crate_path)
}

/// Create the API for interacting with a Substrate runtime.
pub struct SwaggerGenerator {
    metadata: RuntimeMetadataV14,
}

impl SwaggerGenerator {
    /// Create a new runtime generator from the provided metadata.
    ///
    /// **Note:** If you have the metadata path, URL or bytes to hand, prefer to use
    /// one of the `generate_runtime_api_from_*` functions for generating the runtime API
    /// from that.
    pub fn new(metadata: RuntimeMetadataPrefixed) -> Self {
        match metadata.1 {
            RuntimeMetadata::V14(v14) => Self { metadata: v14 },
            _ => panic!("Unsupported metadata version {:?}", metadata.1),
        }
    }
    pub fn generate_swagger(
        &self,
        item_mod: syn::ItemMod,
        crate_path: CratePath,
    ) -> TokenStream2 {
        // TODO: magic to convert the metadata into a swagger spec
        panic!("Not implemented yet");
    }
}
