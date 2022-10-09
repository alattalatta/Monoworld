module DevL10N.TranslationOutput.Utils.Xml

open System.IO
open System.Text
open System.Xml.Linq

open Verse


module XComment =
  let newLine () = XComment("BR")


module XDocument =
  let create (children: XNode seq) =
    XDocument(children)

  let createEmpty () = XDocument()

  let createSingleton (child: XNode) = XDocument([ child ])

  let save path (doc: XDocument) =
    async {
      use file = File.Create(path)

      let contentsString =
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\r\n"
        + doc
          .ToString()
          .Replace("<!--BR-->", "") // exploit the fact that the comments make new lines in .ToString()
          .Replace("&gt;", ">")
        |> Encoding.UTF8.GetBytes

      try
        do! file.AsyncWrite(contentsString, 0, contentsString.Length)
      with
      | ex -> Log.Error(ex.Message)
    }


module XElement =
  let create name (children: XNode seq) =
    XElement(XName.op_Implicit (name), children)

  let createEmpty name = XElement(XName.op_Implicit (name))

  let createSingleton name (child: XNode) =
    XElement(XName.op_Implicit (name), child)
