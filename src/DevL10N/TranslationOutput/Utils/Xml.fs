module DevL10N.TranslationOutput.Utils.Xml

open System.Xml.Linq


module XComment =
  let newLine () = XComment("BR")


module XDocument =
  let create (children: XNode seq) =
    // let childrenWithBreaks =
    //   seq {
    //     yield XComment.newLine () :> XNode
    //     yield! children
    //     yield XComment.newLine () :> XNode
    //   }

    XDocument(children)

  let createEmpty () = XDocument()

  let createSingleton (child: XNode) = XDocument([ child ])


module XElement =
  let create name (children: XNode seq) =
    XElement(XName.op_Implicit (name), children)

  let createEmpty name = XElement(XName.op_Implicit (name))

  let createSingleton name (child: XNode) =
    XElement(XName.op_Implicit (name), child)
