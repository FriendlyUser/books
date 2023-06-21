declare module 'astro:content' {
	interface Render {
		'.mdx': Promise<{
			Content: import('astro').MarkdownInstance<{}>['Content'];
			headings: import('astro').MarkdownHeading[];
			remarkPluginFrontmatter: Record<string, any>;
		}>;
	}
}

declare module 'astro:content' {
	interface Render {
		'.md': Promise<{
			Content: import('astro').MarkdownInstance<{}>['Content'];
			headings: import('astro').MarkdownHeading[];
			remarkPluginFrontmatter: Record<string, any>;
		}>;
	}
}

declare module 'astro:content' {
	export { z } from 'astro/zod';
	export type CollectionEntry<C extends keyof AnyEntryMap> = AnyEntryMap[C][keyof AnyEntryMap[C]];

	// TODO: Remove this when having this fallback is no longer relevant. 2.3? 3.0? - erika, 2023-04-04
	/**
	 * @deprecated
	 * `astro:content` no longer provide `image()`.
	 *
	 * Please use it through `schema`, like such:
	 * ```ts
	 * import { defineCollection, z } from "astro:content";
	 *
	 * defineCollection({
	 *   schema: ({ image }) =>
	 *     z.object({
	 *       image: image(),
	 *     }),
	 * });
	 * ```
	 */
	export const image: never;

	// This needs to be in sync with ImageMetadata
	export type ImageFunction = () => import('astro/zod').ZodObject<{
		src: import('astro/zod').ZodString;
		width: import('astro/zod').ZodNumber;
		height: import('astro/zod').ZodNumber;
		format: import('astro/zod').ZodUnion<
			[
				import('astro/zod').ZodLiteral<'png'>,
				import('astro/zod').ZodLiteral<'jpg'>,
				import('astro/zod').ZodLiteral<'jpeg'>,
				import('astro/zod').ZodLiteral<'tiff'>,
				import('astro/zod').ZodLiteral<'webp'>,
				import('astro/zod').ZodLiteral<'gif'>,
				import('astro/zod').ZodLiteral<'svg'>
			]
		>;
	}>;

	type BaseSchemaWithoutEffects =
		| import('astro/zod').AnyZodObject
		| import('astro/zod').ZodUnion<import('astro/zod').AnyZodObject[]>
		| import('astro/zod').ZodDiscriminatedUnion<string, import('astro/zod').AnyZodObject[]>
		| import('astro/zod').ZodIntersection<
				import('astro/zod').AnyZodObject,
				import('astro/zod').AnyZodObject
		  >;

	type BaseSchema =
		| BaseSchemaWithoutEffects
		| import('astro/zod').ZodEffects<BaseSchemaWithoutEffects>;

	export type SchemaContext = { image: ImageFunction };

	type DataCollectionConfig<S extends BaseSchema> = {
		type: 'data';
		schema?: S | ((context: SchemaContext) => S);
	};

	type ContentCollectionConfig<S extends BaseSchema> = {
		type?: 'content';
		schema?: S | ((context: SchemaContext) => S);
	};

	type CollectionConfig<S> = ContentCollectionConfig<S> | DataCollectionConfig<S>;

	export function defineCollection<S extends BaseSchema>(
		input: CollectionConfig<S>
	): CollectionConfig<S>;

	type AllValuesOf<T> = T extends any ? T[keyof T] : never;
	type ValidContentEntrySlug<C extends keyof ContentEntryMap> = AllValuesOf<
		ContentEntryMap[C]
	>['slug'];

	export function getEntryBySlug<
		C extends keyof ContentEntryMap,
		E extends ValidContentEntrySlug<C> | (string & {})
	>(
		collection: C,
		// Note that this has to accept a regular string too, for SSR
		entrySlug: E
	): E extends ValidContentEntrySlug<C>
		? Promise<CollectionEntry<C>>
		: Promise<CollectionEntry<C> | undefined>;

	export function getDataEntryById<C extends keyof DataEntryMap, E extends keyof DataEntryMap[C]>(
		collection: C,
		entryId: E
	): Promise<CollectionEntry<C>>;

	export function getCollection<C extends keyof AnyEntryMap, E extends CollectionEntry<C>>(
		collection: C,
		filter?: (entry: CollectionEntry<C>) => entry is E
	): Promise<E[]>;
	export function getCollection<C extends keyof AnyEntryMap>(
		collection: C,
		filter?: (entry: CollectionEntry<C>) => unknown
	): Promise<CollectionEntry<C>[]>;

	export function getEntry<
		C extends keyof ContentEntryMap,
		E extends ValidContentEntrySlug<C> | (string & {})
	>(entry: {
		collection: C;
		slug: E;
	}): E extends ValidContentEntrySlug<C>
		? Promise<CollectionEntry<C>>
		: Promise<CollectionEntry<C> | undefined>;
	export function getEntry<
		C extends keyof DataEntryMap,
		E extends keyof DataEntryMap[C] | (string & {})
	>(entry: {
		collection: C;
		id: E;
	}): E extends keyof DataEntryMap[C]
		? Promise<DataEntryMap[C][E]>
		: Promise<CollectionEntry<C> | undefined>;
	export function getEntry<
		C extends keyof ContentEntryMap,
		E extends ValidContentEntrySlug<C> | (string & {})
	>(
		collection: C,
		slug: E
	): E extends ValidContentEntrySlug<C>
		? Promise<CollectionEntry<C>>
		: Promise<CollectionEntry<C> | undefined>;
	export function getEntry<
		C extends keyof DataEntryMap,
		E extends keyof DataEntryMap[C] | (string & {})
	>(
		collection: C,
		id: E
	): E extends keyof DataEntryMap[C]
		? Promise<DataEntryMap[C][E]>
		: Promise<CollectionEntry<C> | undefined>;

	/** Resolve an array of entry references from the same collection */
	export function getEntries<C extends keyof ContentEntryMap>(
		entries: {
			collection: C;
			slug: ValidContentEntrySlug<C>;
		}[]
	): Promise<CollectionEntry<C>[]>;
	export function getEntries<C extends keyof DataEntryMap>(
		entries: {
			collection: C;
			id: keyof DataEntryMap[C];
		}[]
	): Promise<CollectionEntry<C>[]>;

	export function reference<C extends keyof AnyEntryMap>(
		collection: C
	): import('astro/zod').ZodEffects<
		import('astro/zod').ZodString,
		C extends keyof ContentEntryMap
			? {
					collection: C;
					slug: ValidContentEntrySlug<C>;
			  }
			: {
					collection: C;
					id: keyof DataEntryMap[C];
			  }
	>;
	// Allow generic `string` to avoid excessive type errors in the config
	// if `dev` is not running to update as you edit.
	// Invalid collection names will be caught at build time.
	export function reference<C extends string>(
		collection: C
	): import('astro/zod').ZodEffects<import('astro/zod').ZodString, never>;

	type ReturnTypeOrOriginal<T> = T extends (...args: any[]) => infer R ? R : T;
	type InferEntrySchema<C extends keyof AnyEntryMap> = import('astro/zod').infer<
		ReturnTypeOrOriginal<Required<ContentConfig['collections'][C]>['schema']>
	>;

	type ContentEntryMap = {
		"about": {
"index.md": {
	id: "index.md";
  slug: "index";
  body: string;
  collection: "about";
  data: any
} & { render(): Render[".md"] };
};
"blog": {
"-index.md": {
	id: "-index.md";
  slug: "-index";
  body: string;
  collection: "blog";
  data: InferEntrySchema<"blog">
} & { render(): Render[".md"] };
"post-1.mdx": {
	id: "post-1.mdx";
  slug: "post-1";
  body: string;
  collection: "blog";
  data: InferEntrySchema<"blog">
} & { render(): Render[".mdx"] };
"post-10.mdx": {
	id: "post-10.mdx";
  slug: "post-10";
  body: string;
  collection: "blog";
  data: InferEntrySchema<"blog">
} & { render(): Render[".mdx"] };
"post-11.mdx": {
	id: "post-11.mdx";
  slug: "post-11";
  body: string;
  collection: "blog";
  data: InferEntrySchema<"blog">
} & { render(): Render[".mdx"] };
"post-12.mdx": {
	id: "post-12.mdx";
  slug: "post-12";
  body: string;
  collection: "blog";
  data: InferEntrySchema<"blog">
} & { render(): Render[".mdx"] };
"post-2.mdx": {
	id: "post-2.mdx";
  slug: "post-2";
  body: string;
  collection: "blog";
  data: InferEntrySchema<"blog">
} & { render(): Render[".mdx"] };
"post-3.mdx": {
	id: "post-3.mdx";
  slug: "post-3";
  body: string;
  collection: "blog";
  data: InferEntrySchema<"blog">
} & { render(): Render[".mdx"] };
"post-4.mdx": {
	id: "post-4.mdx";
  slug: "post-4";
  body: string;
  collection: "blog";
  data: InferEntrySchema<"blog">
} & { render(): Render[".mdx"] };
"post-5.mdx": {
	id: "post-5.mdx";
  slug: "post-5";
  body: string;
  collection: "blog";
  data: InferEntrySchema<"blog">
} & { render(): Render[".mdx"] };
"post-6.mdx": {
	id: "post-6.mdx";
  slug: "post-6";
  body: string;
  collection: "blog";
  data: InferEntrySchema<"blog">
} & { render(): Render[".mdx"] };
"post-7.mdx": {
	id: "post-7.mdx";
  slug: "post-7";
  body: string;
  collection: "blog";
  data: InferEntrySchema<"blog">
} & { render(): Render[".mdx"] };
"post-8.mdx": {
	id: "post-8.mdx";
  slug: "post-8";
  body: string;
  collection: "blog";
  data: InferEntrySchema<"blog">
} & { render(): Render[".mdx"] };
"post-9.mdx": {
	id: "post-9.mdx";
  slug: "post-9";
  body: string;
  collection: "blog";
  data: InferEntrySchema<"blog">
} & { render(): Render[".mdx"] };
};
"books": {
"-index.md": {
	id: "-index.md";
  slug: "-index";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"COBOL_ref.md": {
	id: "COBOL_ref.md";
  slug: "cobol_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"actionscript_ref.md": {
	id: "actionscript_ref.md";
  slug: "actionscript_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"actix_rust.md": {
	id: "actix_rust.md";
  slug: "actix_rust";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"ada_ref.md": {
	id: "ada_ref.md";
  slug: "ada_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"aging_mind_ref.md": {
	id: "aging_mind_ref.md";
  slug: "aging_mind_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"ai_book_ref.md": {
	id: "ai_book_ref.md";
  slug: "ai_book_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"algorithms_in_csharp.md": {
	id: "algorithms_in_csharp.md";
  slug: "algorithms_in_csharp";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"algorithms_in_golang.md": {
	id: "algorithms_in_golang.md";
  slug: "algorithms_in_golang";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"anger_management.md": {
	id: "anger_management.md";
  slug: "anger_management";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"ansible_ref.md": {
	id: "ansible_ref.md";
  slug: "ansible_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"apache_ref.md": {
	id: "apache_ref.md";
  slug: "apache_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"archery_ref.md": {
	id: "archery_ref.md";
  slug: "archery_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"art_of_war.md": {
	id: "art_of_war.md";
  slug: "art_of_war";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"aspnet_ref.md": {
	id: "aspnet_ref.md";
  slug: "aspnet_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"autohotkey_ref.md": {
	id: "autohotkey_ref.md";
  slug: "autohotkey_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"bash_book.md": {
	id: "bash_book.md";
  slug: "bash_book";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"beego.md": {
	id: "beego.md";
  slug: "beego";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"bevy_ref.md": {
	id: "bevy_ref.md";
  slug: "bevy_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"blades_bows_ref.md": {
	id: "blades_bows_ref.md";
  slug: "blades_bows_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"c_main.md": {
	id: "c_main.md";
  slug: "c_main";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"cakephp_ref.md": {
	id: "cakephp_ref.md";
  slug: "cakephp_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"catalyst_ref.md": {
	id: "catalyst_ref.md";
  slug: "catalyst_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"chef_ref.md": {
	id: "chef_ref.md";
  slug: "chef_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"chicagoboss_ref.md": {
	id: "chicagoboss_ref.md";
  slug: "chicagoboss_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"civilization_dawn_ref.md": {
	id: "civilization_dawn_ref.md";
  slug: "civilization_dawn_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"clojure_ref.md": {
	id: "clojure_ref.md";
  slug: "clojure_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"cmake_ref.md": {
	id: "cmake_ref.md";
  slug: "cmake_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"coffeescript.md": {
	id: "coffeescript.md";
  slug: "coffeescript";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"coldfusion_ref.md": {
	id: "coldfusion_ref.md";
  slug: "coldfusion_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"cpp.md": {
	id: "cpp.md";
  slug: "cpp";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"cpp_cli_main_ref.md": {
	id: "cpp_cli_main_ref.md";
  slug: "cpp_cli_main_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"csharp_angular_ref.md": {
	id: "csharp_angular_ref.md";
  slug: "csharp_angular_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"csharp_react_ref.md": {
	id: "csharp_react_ref.md";
  slug: "csharp_react_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"dart_cli_main.md": {
	id: "dart_cli_main.md";
  slug: "dart_cli_main";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"data_science_ref.md": {
	id: "data_science_ref.md";
  slug: "data_science_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"dating_ref.md": {
	id: "dating_ref.md";
  slug: "dating_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"designing_ref.md": {
	id: "designing_ref.md";
  slug: "designing_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"django_ref.md": {
	id: "django_ref.md";
  slug: "django_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"docker_ref.md": {
	id: "docker_ref.md";
  slug: "docker_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"elasticsearch_ref.md": {
	id: "elasticsearch_ref.md";
  slug: "elasticsearch_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"electron_ref.md": {
	id: "electron_ref.md";
  slug: "electron_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"elixir.md": {
	id: "elixir.md";
  slug: "elixir";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"erlang_ref.md": {
	id: "erlang_ref.md";
  slug: "erlang_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"erlang_scrapping.md": {
	id: "erlang_scrapping.md";
  slug: "erlang_scrapping";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"fastapi_ref.md": {
	id: "fastapi_ref.md";
  slug: "fastapi_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"filemaker_ref.md": {
	id: "filemaker_ref.md";
  slug: "filemaker_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"firearms_ref.md": {
	id: "firearms_ref.md";
  slug: "firearms_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"flask_ref.md": {
	id: "flask_ref.md";
  slug: "flask_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"flutter.md": {
	id: "flutter.md";
  slug: "flutter";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"flutter_ref.md": {
	id: "flutter_ref.md";
  slug: "flutter_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"fortran_ref.md": {
	id: "fortran_ref.md";
  slug: "fortran_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"getting_started_with_angular_main.md": {
	id: "getting_started_with_angular_main.md";
  slug: "getting_started_with_angular_main";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"git.md": {
	id: "git.md";
  slug: "git";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"go_cli_main_ref.md": {
	id: "go_cli_main_ref.md";
  slug: "go_cli_main_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"go_gin_ref.md": {
	id: "go_gin_ref.md";
  slug: "go_gin_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"go_scrapping_ref.md": {
	id: "go_scrapping_ref.md";
  slug: "go_scrapping_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"goals_ref.md": {
	id: "goals_ref.md";
  slug: "goals_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"golang_selenium_ref.md": {
	id: "golang_selenium_ref.md";
  slug: "golang_selenium_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"gold_book_ref.md": {
	id: "gold_book_ref.md";
  slug: "gold_book_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"google_sheets_ref.md": {
	id: "google_sheets_ref.md";
  slug: "google_sheets_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"graphql_go.md": {
	id: "graphql_go.md";
  slug: "graphql_go";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"graphql_ref.md": {
	id: "graphql_ref.md";
  slug: "graphql_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"groovy_ref.md": {
	id: "groovy_ref.md";
  slug: "groovy_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"haskell_ref.md": {
	id: "haskell_ref.md";
  slug: "haskell_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"healing_from_loss_ref.md": {
	id: "healing_from_loss_ref.md";
  slug: "healing_from_loss_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"history_of_gold_ref.md": {
	id: "history_of_gold_ref.md";
  slug: "history_of_gold_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"hugo_ref.md": {
	id: "hugo_ref.md";
  slug: "hugo_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"human_nature_ref.md": {
	id: "human_nature_ref.md";
  slug: "human_nature_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"idris_ref.md": {
	id: "idris_ref.md";
  slug: "idris_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"intro_scala_ref.md": {
	id: "intro_scala_ref.md";
  slug: "intro_scala_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"intro_to_pp.md": {
	id: "intro_to_pp.md";
  slug: "intro_to_pp";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"intro_to_ruby.md": {
	id: "intro_to_ruby.md";
  slug: "intro_to_ruby";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"java_book.md": {
	id: "java_book.md";
  slug: "java_book";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"java_cli_main_ref.md": {
	id: "java_cli_main_ref.md";
  slug: "java_cli_main_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"java_intro_ref.md": {
	id: "java_intro_ref.md";
  slug: "java_intro_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"javascript.md": {
	id: "javascript.md";
  slug: "javascript";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"jenkins_ref.md": {
	id: "jenkins_ref.md";
  slug: "jenkins_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"jquery_ref.md": {
	id: "jquery_ref.md";
  slug: "jquery_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"js_ts_other_ref.md": {
	id: "js_ts_other_ref.md";
  slug: "js_ts_other_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"knights_shining_ref.md": {
	id: "knights_shining_ref.md";
  slug: "knights_shining_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"kotlin_book.md": {
	id: "kotlin_book.md";
  slug: "kotlin_book";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"kotlin_cli_ref.md": {
	id: "kotlin_cli_ref.md";
  slug: "kotlin_cli_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"kotlin_spring_ref.md": {
	id: "kotlin_spring_ref.md";
  slug: "kotlin_spring_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"laravel_ref.md": {
	id: "laravel_ref.md";
  slug: "laravel_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"make_business_ref.md": {
	id: "make_business_ref.md";
  slug: "make_business_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"marketing_ref.md": {
	id: "marketing_ref.md";
  slug: "marketing_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"medieval_times.md": {
	id: "medieval_times.md";
  slug: "medieval_times";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"mindset_ref.md": {
	id: "mindset_ref.md";
  slug: "mindset_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"mojolicious_ref.md": {
	id: "mojolicious_ref.md";
  slug: "mojolicious_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"mongo_db_type_ref.md": {
	id: "mongo_db_type_ref.md";
  slug: "mongo_db_type_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"morning_miracle_start.md": {
	id: "morning_miracle_start.md";
  slug: "morning_miracle_start";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"motiviation.md": {
	id: "motiviation.md";
  slug: "motiviation";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"nextjs_ref.md": {
	id: "nextjs_ref.md";
  slug: "nextjs_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"nginx_ref.md": {
	id: "nginx_ref.md";
  slug: "nginx_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"node_back_end.md": {
	id: "node_back_end.md";
  slug: "node_back_end";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"nosql_golang_ref.md": {
	id: "nosql_golang_ref.md";
  slug: "nosql_golang_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"objective_c_ref.md": {
	id: "objective_c_ref.md";
  slug: "objective_c_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"oil_ref.md": {
	id: "oil_ref.md";
  slug: "oil_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"personal_finance_main.md": {
	id: "personal_finance_main.md";
  slug: "personal_finance_main";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"phil_ref.md": {
	id: "phil_ref.md";
  slug: "phil_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"phoenix_ref.md": {
	id: "phoenix_ref.md";
  slug: "phoenix_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"ponzi_ref.md": {
	id: "ponzi_ref.md";
  slug: "ponzi_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"powershell.md": {
	id: "powershell.md";
  slug: "powershell";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"python_pygame_ref.md": {
	id: "python_pygame_ref.md";
  slug: "python_pygame_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"python_ta.md": {
	id: "python_ta.md";
  slug: "python_ta";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"rails_ruby_ref.md": {
	id: "rails_ruby_ref.md";
  slug: "rails_ruby_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"react_projects_ref.md": {
	id: "react_projects_ref.md";
  slug: "react_projects_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"realestaet_ref.md": {
	id: "realestaet_ref.md";
  slug: "realestaet_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"redis_ref.md": {
	id: "redis_ref.md";
  slug: "redis_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"rome.md": {
	id: "rome.md";
  slug: "rome";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"rust.md": {
	id: "rust.md";
  slug: "rust";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"rust_cli_main.md": {
	id: "rust_cli_main.md";
  slug: "rust_cli_main";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"samurai_ref.md": {
	id: "samurai_ref.md";
  slug: "samurai_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"samurai_sword_ref.md": {
	id: "samurai_sword_ref.md";
  slug: "samurai_sword_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"scala_main.md": {
	id: "scala_main.md";
  slug: "scala_main";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"sfml_cpp_ref.md": {
	id: "sfml_cpp_ref.md";
  slug: "sfml_cpp_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"siege_data_ref.md": {
	id: "siege_data_ref.md";
  slug: "siege_data_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"sins_ref.md": {
	id: "sins_ref.md";
  slug: "sins_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"solidity_ref.md": {
	id: "solidity_ref.md";
  slug: "solidity_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"spring_boot_ref.md": {
	id: "spring_boot_ref.md";
  slug: "spring_boot_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"sql_ref.md": {
	id: "sql_ref.md";
  slug: "sql_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"tailwind_css.md": {
	id: "tailwind_css.md";
  slug: "tailwind_css";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"typescript.md": {
	id: "typescript.md";
  slug: "typescript";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"typescript_cli_main_ref.md": {
	id: "typescript_cli_main_ref.md";
  slug: "typescript_cli_main_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"typescript_express.md": {
	id: "typescript_express.md";
  slug: "typescript_express";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"typescript_koa_ref.md": {
	id: "typescript_koa_ref.md";
  slug: "typescript_koa_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"typescript_scrapping_ref.md": {
	id: "typescript_scrapping_ref.md";
  slug: "typescript_scrapping_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"ui_ux_ref.md": {
	id: "ui_ux_ref.md";
  slug: "ui_ux_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"v_lang_ref.md": {
	id: "v_lang_ref.md";
  slug: "v_lang_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"vb_net_ref.md": {
	id: "vb_net_ref.md";
  slug: "vb_net_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"video_games_ref.md": {
	id: "video_games_ref.md";
  slug: "video_games_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"vikings.md": {
	id: "vikings.md";
  slug: "vikings";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"vue_full.md": {
	id: "vue_full.md";
  slug: "vue_full";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"web3_ethereum_ref.md": {
	id: "web3_ethereum_ref.md";
  slug: "web3_ethereum_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"westcove.md": {
	id: "westcove.md";
  slug: "westcove";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"wordpress_main.md": {
	id: "wordpress_main.md";
  slug: "wordpress_main";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
"zos_ref.md": {
	id: "zos_ref.md";
  slug: "zos_ref";
  body: string;
  collection: "books";
  data: InferEntrySchema<"books">
} & { render(): Render[".md"] };
};
"careers": {
"-index.mdx": {
	id: "-index.mdx";
  slug: "-index";
  body: string;
  collection: "careers";
  data: any
} & { render(): Render[".mdx"] };
"design-lead.md": {
	id: "design-lead.md";
  slug: "design-lead";
  body: string;
  collection: "careers";
  data: any
} & { render(): Render[".md"] };
"full-stack-developer.md": {
	id: "full-stack-developer.md";
  slug: "full-stack-developer";
  body: string;
  collection: "careers";
  data: any
} & { render(): Render[".md"] };
"head-desigin.md": {
	id: "head-desigin.md";
  slug: "head-desigin";
  body: string;
  collection: "careers";
  data: any
} & { render(): Render[".md"] };
"head-of-inbound-marketing.md": {
	id: "head-of-inbound-marketing.md";
  slug: "head-of-inbound-marketing";
  body: string;
  collection: "careers";
  data: any
} & { render(): Render[".md"] };
"junior-developer.md": {
	id: "junior-developer.md";
  slug: "junior-developer";
  body: string;
  collection: "careers";
  data: any
} & { render(): Render[".md"] };
"laravel-developer.md": {
	id: "laravel-developer.md";
  slug: "laravel-developer";
  body: string;
  collection: "careers";
  data: any
} & { render(): Render[".md"] };
"lead-product-designer.md": {
	id: "lead-product-designer.md";
  slug: "lead-product-designer";
  body: string;
  collection: "careers";
  data: any
} & { render(): Render[".md"] };
};
"contact": {
"index.md": {
	id: "index.md";
  slug: "index";
  body: string;
  collection: "contact";
  data: any
} & { render(): Render[".md"] };
};
"homepage": {
"index.md": {
	id: "index.md";
  slug: "index";
  body: string;
  collection: "homepage";
  data: any
} & { render(): Render[".md"] };
};
"how-it-works": {
"index.md": {
	id: "index.md";
  slug: "index";
  body: string;
  collection: "how-it-works";
  data: any
} & { render(): Render[".md"] };
};
"integrations": {
"-index.mdx": {
	id: "-index.mdx";
  slug: "-index";
  body: string;
  collection: "integrations";
  data: any
} & { render(): Render[".mdx"] };
"facebook.mdx": {
	id: "facebook.mdx";
  slug: "facebook";
  body: string;
  collection: "integrations";
  data: any
} & { render(): Render[".mdx"] };
"google.mdx": {
	id: "google.mdx";
  slug: "google";
  body: string;
  collection: "integrations";
  data: any
} & { render(): Render[".mdx"] };
"pinterest.mdx": {
	id: "pinterest.mdx";
  slug: "pinterest";
  body: string;
  collection: "integrations";
  data: any
} & { render(): Render[".mdx"] };
"twitter.mdx": {
	id: "twitter.mdx";
  slug: "twitter";
  body: string;
  collection: "integrations";
  data: any
} & { render(): Render[".mdx"] };
"webflow.mdx": {
	id: "webflow.mdx";
  slug: "webflow";
  body: string;
  collection: "integrations";
  data: any
} & { render(): Render[".mdx"] };
"youtube.mdx": {
	id: "youtube.mdx";
  slug: "youtube";
  body: string;
  collection: "integrations";
  data: any
} & { render(): Render[".mdx"] };
};
"pages": {
"404.md": {
	id: "404.md";
  slug: "404";
  body: string;
  collection: "pages";
  data: InferEntrySchema<"pages">
} & { render(): Render[".md"] };
"changelog.mdx": {
	id: "changelog.mdx";
  slug: "changelog";
  body: string;
  collection: "pages";
  data: InferEntrySchema<"pages">
} & { render(): Render[".mdx"] };
"elements.mdx": {
	id: "elements.mdx";
  slug: "elements";
  body: string;
  collection: "pages";
  data: InferEntrySchema<"pages">
} & { render(): Render[".mdx"] };
"terms-and-conditions.md": {
	id: "terms-and-conditions.md";
  slug: "terms-and-conditions";
  body: string;
  collection: "pages";
  data: InferEntrySchema<"pages">
} & { render(): Render[".md"] };
};
"pricing": {
"index.mdx": {
	id: "index.mdx";
  slug: "index";
  body: string;
  collection: "pricing";
  data: any
} & { render(): Render[".mdx"] };
};

	};

	type DataEntryMap = {
		
	};

	type AnyEntryMap = ContentEntryMap & DataEntryMap;

	type ContentConfig = typeof import("../src/content/config");
}
