open System
open System.IO
open FSharp.NativeInterop
open System.Collections.Generic

#nowarn "9"

let inline stackalloc<'a when 'a: unmanaged> (length: int) : Span<'a> =
    let p =
        NativePtr.stackalloc<'a> length
        |> NativePtr.toVoidPtr

    Span<'a>(p, length)


[<Struct>]
type Post =
    { _id: string
      title: string
      tags: string [] }

[<Struct>]
type RelatedPosts =
    { _id: string
      tags: string []
      related: Post [] }

let srcDir = __SOURCE_DIRECTORY__

module Serialization =
    open System.Text.Json.Serialization.Metadata
    open System.Text.Json
    open System.Text.Json.Serialization

    type Writer<'t> = Utf8JsonWriter -> 't -> unit

    let serializeArray (writeFn: Writer<'t>) (writer: Utf8JsonWriter) (values: 't []) =
        if values = null then
            writer.WriteNullValue()
        else
            writer.WriteStartArray()

            for value in values do
                writeFn writer value

            writer.WriteEndArray()


    let serializationOptions = JsonSerializerOptions()

    let writePost: Writer<Post> =
        fun writer post ->
            writer.WriteStartObject()
            writer.WriteString("_id", post._id)
            writer.WriteString("title", post.title)
            writer.WritePropertyName "tags"
            serializeArray (fun (writer: Utf8JsonWriter) (tag: string) -> writer.WriteStringValue tag) writer post.tags
            writer.WriteEndObject()

    let postInfo =
        let info =
            JsonObjectInfoValues<Post>(
                ObjectCreator =
                    (fun () ->
                        { _id = null
                          title = null
                          tags = null }),
                ObjectWithParameterizedConstructorCreator = null,
                PropertyMetadataInitializer = null,
                ConstructorParameterMetadataInitializer =
                    (fun _ ->
                        [| JsonParameterInfoValues(Name = "_id", ParameterType = typeof<string>, Position = 0)
                           JsonParameterInfoValues(Name = "title", ParameterType = typeof<string>, Position = 1)
                           JsonParameterInfoValues(Name = "tags", ParameterType = typeof<string []>, Position = 2) |]),
                NumberHandling = JsonNumberHandling.Strict,
                SerializeHandler = writePost
            )

        JsonMetadataServices.CreateObjectInfo<Post>(serializationOptions, info)

    let postsInfo =
        let postsInfo: JsonCollectionInfoValues<Post []> =
            JsonCollectionInfoValues<Post []>(
                ObjectCreator = null,
                KeyInfo = null,
                ElementInfo = postInfo,
                NumberHandling = JsonNumberHandling.Strict,
                SerializeHandler = Action<_, _>(serializeArray writePost)
            )

        JsonMetadataServices.CreateArrayInfo<Post>(serializationOptions, postsInfo)

    let writeRelatedPost: Writer<RelatedPosts> =
        fun writer post ->
            writer.WriteStartObject()
            writer.WriteString("_id", post._id)
            writer.WritePropertyName "related"
            serializeArray writePost writer post.related
            writer.WritePropertyName "tags"

            serializeArray (fun (writer: Utf8JsonWriter) (tag: string) -> writer.WriteStringValue tag) writer post.tags


    let relatedPostInfo =

        let objectInfo =
            new JsonObjectInfoValues<RelatedPosts>(
                ObjectCreator =
                    (fun () ->
                        { _id = null
                          tags = null
                          related = null }),
                ObjectWithParameterizedConstructorCreator = null,
                PropertyMetadataInitializer = null,
                ConstructorParameterMetadataInitializer =
                    (fun () ->
                        [| JsonParameterInfoValues(Name = "_id", ParameterType = typeof<string>, Position = 0)
                           JsonParameterInfoValues(Name = "tags", ParameterType = typeof<string []>, Position = 1)
                           JsonParameterInfoValues(Name = "related", ParameterType = typeof<Post []>, Position = 2) |]),
                NumberHandling = JsonNumberHandling.Strict,
                SerializeHandler = Action<_, _>(writeRelatedPost)
            )

        JsonMetadataServices.CreateObjectInfo<RelatedPosts>(serializationOptions, objectInfo)

    let relatedPostsInfo =
        let postsInfo: JsonCollectionInfoValues<RelatedPosts []> =
            JsonCollectionInfoValues<RelatedPosts []>(
                ObjectCreator = null,
                KeyInfo = null,
                ElementInfo = relatedPostInfo,
                NumberHandling = JsonNumberHandling.Strict,
                SerializeHandler = Action<_, _>(serializeArray writeRelatedPost)
            )

        JsonMetadataServices.CreateArrayInfo<RelatedPosts>(serializationOptions, postsInfo)

    let stringInfo =
        JsonMetadataServices.CreateValueInfo<string>(serializationOptions, JsonMetadataServices.StringConverter)

    let stringArrayInfo =
        let info =
            JsonCollectionInfoValues<System.String []>(

                ObjectCreator = null,
                KeyInfo = null,
                ElementInfo = stringInfo,
                NumberHandling = JsonNumberHandling.Strict,
                SerializeHandler =
                    serializeArray (fun (writer: Utf8JsonWriter) (value: string) -> writer.WriteStringValue value)
            )

        JsonMetadataServices.CreateArrayInfo<string>(serializationOptions, info)


    serializationOptions.TypeInfoResolver <-
        JsonTypeInfoResolver.Combine(
            { new IJsonTypeInfoResolver with
                member this.GetTypeInfo(``type``: Type, options: JsonSerializerOptions) : JsonTypeInfo =
                    Console.WriteLine("looking up type: {0}", ``type``.FullName)

                    if ``type`` = typeof<Post> then
                        postInfo
                    elif ``type`` = typeof<RelatedPosts> then
                        relatedPostInfo
                    elif ``type`` = typeof<Post []> then
                        postsInfo
                    elif ``type`` = typeof<RelatedPosts []> then
                        relatedPostsInfo
                    elif ``type`` = typeof<string []> then
                        stringArrayInfo
                    elif ``type`` = typeof<string> then
                        stringInfo
                    else
                        null }
        )

    let parsePosts path =
        JsonSerializer.Deserialize(File.OpenRead(path), postsInfo)

    let savePosts path posts =
        JsonSerializer.Serialize(File.OpenWrite(path), posts, relatedPostsInfo)

module Chiron =
    open Chiron
    open Chiron.Serialization.Json

    let decodePost: Decoder<Json, Post> =
        let mk i t tags = { _id = i; title = t; tags = tags }

        let props =
            Decoder.map3
                mk
                (Decode.required Decode.string "_id")
                (Decode.required Decode.string "title")
                (Decode.required (Decode.arrayWith Decode.string) "tags")

        Decode.jsonObjectWith props

    let decodePostArray = Decode.arrayWith decodePost

    let encodePost =
        let props (post: Post) jobj =
            jobj
            |> Encode.required Encode.string "_id" post._id
            |> Encode.required Encode.string "title" post.title
            |> Encode.required (Encode.arrayWith Encode.string) "tags" post.tags

        Encode.jsonObjectWith props

    let encodeRelatedPost =
        let props (post: RelatedPosts) jobj =
            jobj
            |> Encode.required Encode.string "_id" post._id
            |> Encode.required (Encode.arrayWith Encode.string) "tags" post.tags
            |> Encode.required (Encode.arrayWith encodePost) "related" post.related

        Encode.jsonObjectWith props

    let encodeRelatedPosts = Encode.arrayWith encodeRelatedPost

    let parsePosts path =
        Parsing.Json.parseStream (File.OpenRead(path))
        |> JsonResult.bind decodePostArray
        |> JsonResult.getOrThrow

    let savePosts path posts =
        use writeStream = File.OpenWrite(path)

        use writer = new StreamWriter(writeStream)

        Json.serializeWith encodeRelatedPosts JsonFormattingOptions.Compact posts
        |> writer.WriteLine


let posts =
    let path = Path.Combine(srcDir, "../posts.json")
    //Chiron.parsePosts path
    Serialization.parsePosts path

let stopwatch = Diagnostics.Stopwatch()
stopwatch.Start()

// Start work
let tagPostsTmp = Dictionary<string, Stack<int>>()

posts
|> Array.iteri (fun postId post ->

    for tag in post.tags do

        match tagPostsTmp.TryGetValue tag with
        | true, s -> s.Push postId
        | false, _ ->
            let newStack = Stack()
            newStack.Push postId
            tagPostsTmp[tag] <- newStack)

// convert from Dict<_,Stack<int>> to Dict<_,int[]> for faster access
let tagPosts = Dictionary(tagPostsTmp.Count)

for kv in tagPostsTmp do
    tagPosts[kv.Key] <- kv.Value.ToArray()

let topN = 5

let allRelatedPosts: RelatedPosts [] =
    posts
    |> Array.mapi (fun postId post ->
        let taggedPostCount = stackalloc posts.Length
        let top5 = stackalloc (topN * 2) // flattened list of (count, id)

        for tagId in post.tags do
            for relatedPostId in tagPosts[tagId] do
                taggedPostCount[relatedPostId] <- taggedPostCount[relatedPostId] + 1

        taggedPostCount[postId] <- 0 // ignore self

        let mutable minTags = 0

        // custom priority queue to find topN
        for i in 0 .. taggedPostCount.Length - 1 do
            let count = taggedPostCount[i]

            if count > minTags then
                // Find upper bound: pos at which count is larger than current one.
                let mutable upperBound = (topN - 2) * 2

                while upperBound >= 0 && count > top5[upperBound] do
                    top5[upperBound + 2] <- top5[upperBound]
                    top5[upperBound + 3] <- top5[upperBound + 1]
                    upperBound <- upperBound - 2

                let insertionPos = upperBound + 2
                top5[insertionPos] <- count
                top5[insertionPos + 1] <- i

                minTags <- top5[topN * 2 - 2]

        let related = Array.zeroCreate topN

        for i in 0 .. related.Length - 1 do
            related[i] <- posts[top5[i * 2 + 1]]

        { _id = post._id
          tags = post.tags
          related = related }

    )


stopwatch.Stop()
System.Console.WriteLine("Processing time (w/o IO): {0}ms", stopwatch.ElapsedMilliseconds)

let path = Path.Combine(srcDir, "../related_posts_fsharp.json")
Serialization.savePosts path allRelatedPosts
