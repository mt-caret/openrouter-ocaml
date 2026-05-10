(** Snapshot tests for [Completions.Request] wire format. Each test builds a
    request via the smart constructors and prints the JSON we'd send to
    OpenRouter — the snapshot guards against silent encoding regressions. *)

open! Core
open Openrouter_api
open Completions

let print_non_streaming r =
  [%jsonaf_of: [ `Non_streaming ] Request.t] r |> Jsonaf.to_string_hum |> print_endline
;;

let print_streaming r =
  [%jsonaf_of: [ `Streaming ] Request.t] r |> Jsonaf.to_string_hum |> print_endline
;;

let%expect_test "request: minimal" =
  Request.create
    ~model:"openai/gpt-4o"
    ~messages:[ Request.Message.user "Hello, world!" ]
    ()
  |> print_non_streaming;
  [%expect
    {|
    {
      "model": "openai/gpt-4o",
      "messages": [
        {
          "role": "user",
          "content": "Hello, world!"
        }
      ],
      "stream": false
    }
    |}]
;;

let%expect_test "request: with function tool + tool_choice = auto" =
  let search_tool =
    Tool.function_
      ~name:"search_books"
      ~description:"Search for books in a library"
      ~parameters:
        (`Object
            [ "type", `String "object"
            ; ( "properties"
              , `Object
                  [ ( "query"
                    , `Object
                        [ "type", `String "string"
                        ; "description", `String "Search query"
                        ] )
                  ] )
            ; "required", `Array [ `String "query" ]
            ])
      ()
  in
  Request.create
    ~model:"google/gemini-2.0-flash-001"
    ~messages:[ Request.Message.user "Find books about OCaml" ]
    ~tools:[ search_tool ]
    ~tool_choice:Tool_choice.auto
    ()
  |> print_non_streaming;
  [%expect
    {|
    {
      "model": "google/gemini-2.0-flash-001",
      "messages": [
        {
          "role": "user",
          "content": "Find books about OCaml"
        }
      ],
      "stream": false,
      "tools": [
        {
          "type": "function",
          "function": {
            "name": "search_books",
            "description": "Search for books in a library",
            "parameters": {
              "type": "object",
              "properties": {
                "query": {
                  "type": "string",
                  "description": "Search query"
                }
              },
              "required": [
                "query"
              ]
            }
          }
        }
      ],
      "tool_choice": "auto"
    }
    |}]
;;

let%expect_test "request: tool result message (assistant tool_call → tool reply)" =
  Request.create
    ~model:"openai/gpt-4o"
    ~messages:
      [ Request.Message.user "Find books about OCaml"
      ; Request.Message.assistant
          ~tool_calls:
            [ { id = "call_abc123"
              ; type_ = "function"
              ; function_ = { name = "search_books"; arguments = {|{"query": "OCaml"}|} }
              }
            ]
          ()
      ; Request.Message.tool
          ~tool_call_id:"call_abc123"
          ~content:{|[{"title": "Real World OCaml", "author": "Yaron Minsky"}]|}
      ]
    ()
  |> print_non_streaming;
  [%expect
    {|
    {
      "model": "openai/gpt-4o",
      "messages": [
        {
          "role": "user",
          "content": "Find books about OCaml"
        },
        {
          "role": "assistant",
          "tool_calls": [
            {
              "id": "call_abc123",
              "type": "function",
              "function": {
                "name": "search_books",
                "arguments": "{\"query\": \"OCaml\"}"
              }
            }
          ]
        },
        {
          "role": "tool",
          "content": "[{\"title\": \"Real World OCaml\", \"author\": \"Yaron Minsky\"}]",
          "tool_call_id": "call_abc123"
        }
      ],
      "stream": false
    }
    |}]
;;

let%expect_test "request: streaming with web-search plugin" =
  Request.create_streaming
    ~model:"openai/gpt-4o"
    ~messages:[ Request.Message.user "What's happening in AI today?" ]
    ~plugins:[ Plugin.web () ]
    ()
  |> print_streaming;
  [%expect
    {|
    {
      "model": "openai/gpt-4o",
      "messages": [
        {
          "role": "user",
          "content": "What's happening in AI today?"
        }
      ],
      "stream": true,
      "plugins": [
        {
          "id": "web"
        }
      ]
    }
    |}]
;;

let%expect_test "request: file attachment via multipart" =
  Request.create
    ~model:"anthropic/claude-sonnet-4"
    ~messages:
      [ Request.Message.user_multipart
          [ Request.Message.Content_part.text "What are the main points in this document?"
          ; Request.Message.Content_part.file
              ~filename:"document.pdf"
              ~file_data:"https://bitcoin.org/bitcoin.pdf"
              ()
          ]
      ]
    ()
  |> print_non_streaming;
  [%expect
    {|
    {
      "model": "anthropic/claude-sonnet-4",
      "messages": [
        {
          "role": "user",
          "content": [
            {
              "type": "text",
              "text": "What are the main points in this document?"
            },
            {
              "type": "file",
              "file": {
                "filename": "document.pdf",
                "file_data": "https://bitcoin.org/bitcoin.pdf"
              }
            }
          ]
        }
      ],
      "stream": false
    }
    |}]
;;

let%expect_test "request: file-parser plugin (mistral_ocr)" =
  Request.create
    ~model:"anthropic/claude-sonnet-4"
    ~messages:
      [ Request.Message.user_multipart
          [ Request.Message.Content_part.text "Summarize this PDF"
          ; Request.Message.Content_part.file
              ~filename:"document.pdf"
              ~file_data:"data:application/pdf;base64,JVBERi0xLjQ..."
              ()
          ]
      ]
    ~plugins:[ Plugin.file_parser ~pdf_engine:Mistral_ocr () ]
    ()
  |> print_non_streaming;
  [%expect
    {|
    {
      "model": "anthropic/claude-sonnet-4",
      "messages": [
        {
          "role": "user",
          "content": [
            {
              "type": "text",
              "text": "Summarize this PDF"
            },
            {
              "type": "file",
              "file": {
                "filename": "document.pdf",
                "file_data": "data:application/pdf;base64,JVBERi0xLjQ..."
              }
            }
          ]
        }
      ],
      "stream": false,
      "plugins": [
        {
          "id": "file-parser",
          "pdf": {
            "engine": "mistral-ocr"
          }
        }
      ]
    }
    |}]
;;

let%expect_test "request: audio + video content parts" =
  Request.create
    ~model:"google/gemini-2.5-flash"
    ~messages:
      [ Request.Message.user_multipart
          [ Request.Message.Content_part.text "transcribe this"
          ; Request.Message.Content_part.audio
              ~format:"mp3"
              ~data:"AAAA-fake-base64-audio"
              ()
          ; Request.Message.Content_part.video_url ~url:"https://example.com/clip.mp4" ()
          ; Request.Message.Content_part.video_base64
              ~mime_type:"video/mp4"
              ~data:"AAAA-fake-base64-video"
              ()
          ]
      ]
    ()
  |> print_non_streaming;
  [%expect
    {|
    {
      "model": "google/gemini-2.5-flash",
      "messages": [
        {
          "role": "user",
          "content": [
            {
              "type": "text",
              "text": "transcribe this"
            },
            {
              "type": "input_audio",
              "input_audio": {
                "data": "AAAA-fake-base64-audio",
                "format": "mp3"
              }
            },
            {
              "type": "video_url",
              "video_url": {
                "url": "https://example.com/clip.mp4"
              }
            },
            {
              "type": "video_url",
              "video_url": {
                "url": "data:video/mp4;base64,AAAA-fake-base64-video"
              }
            }
          ]
        }
      ],
      "stream": false
    }
    |}]
;;

let%expect_test "request: cache_control on content parts" =
  Request.create
    ~model:"anthropic/claude-sonnet-4"
    ~messages:
      [ Request.Message.user_multipart
          [ Request.Message.Content_part.text
              ~cache_control:(Request.Message.Content_part.Cache_control.ephemeral ())
              "<long system prompt that should be cached>"
          ; Request.Message.Content_part.text
              ~cache_control:
                (Request.Message.Content_part.Cache_control.ephemeral ~ttl:"1h" ())
              "<another cached block, with explicit ttl>"
          ; Request.Message.Content_part.text "what's the weather?"
          ]
      ]
    ()
  |> print_non_streaming;
  [%expect
    {|
    {
      "model": "anthropic/claude-sonnet-4",
      "messages": [
        {
          "role": "user",
          "content": [
            {
              "type": "text",
              "text": "<long system prompt that should be cached>",
              "cache_control": {
                "type": "ephemeral"
              }
            },
            {
              "type": "text",
              "text": "<another cached block, with explicit ttl>",
              "cache_control": {
                "type": "ephemeral",
                "ttl": "1h"
              }
            },
            {
              "type": "text",
              "text": "what's the weather?"
            }
          ]
        }
      ],
      "stream": false
    }
    |}]
;;

let%expect_test "request: provider routing (every Provider.t field)" =
  let provider : Request.Provider.t =
    { order = Some [ "DeepInfra"; "Lambda" ]
    ; allow_fallbacks = Some false
    ; require_parameters = Some true
    ; data_collection = Some Deny
    ; zdr = Some true
    ; only = Some [ "DeepInfra"; "Lambda" ]
    ; ignore = None
    ; quantizations = Some [ "fp8" ]
    ; sort = Some Price
    ; max_price =
        Some { prompt = Some 1.0; completion = Some 2.0; request = None; image = None }
    ; preferred_min_throughput = Some 30.0
    ; preferred_max_latency = Some 5.0
    }
  in
  Request.create
    ~model:"meta-llama/llama-3.3-70b-instruct"
    ~messages:[ Request.Message.user "hi" ]
    ~provider
    ()
  |> print_non_streaming;
  [%expect
    {|
    {
      "model": "meta-llama/llama-3.3-70b-instruct",
      "messages": [
        {
          "role": "user",
          "content": "hi"
        }
      ],
      "stream": false,
      "provider": {
        "order": [
          "DeepInfra",
          "Lambda"
        ],
        "allow_fallbacks": false,
        "require_parameters": true,
        "data_collection": "deny",
        "zdr": true,
        "only": [
          "DeepInfra",
          "Lambda"
        ],
        "quantizations": [
          "fp8"
        ],
        "sort": "price",
        "max_price": {
          "prompt": 1,
          "completion": 2
        },
        "preferred_min_throughput": 30,
        "preferred_max_latency": 5
      }
    }
    |}]
;;
