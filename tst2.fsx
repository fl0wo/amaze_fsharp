open System

// This is our Unicode string:
let s_unicode: string = "abcéabc"

// Convert a string to utf-8 bytes.
let utf8Bytes: array<byte> = System.Text.Encoding.UTF8.GetBytes(s_unicode)

// Convert utf-8 bytes to a string.
let s_unicode2: string = System.Text.Encoding.UTF8.GetString(utf8Bytes)

Console.WriteLine(s_unicode2)
