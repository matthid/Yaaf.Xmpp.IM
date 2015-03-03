// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
/// parsing logic for XEP-0297: Stanza Forwarding (http://xmpp.org/extensions/xep-0297.html)
namespace Yaaf.Xmpp.IM.StanzaForwarding

open System
open Yaaf.Xmpp
open Yaaf.Xmpp.IM
open Yaaf.Xmpp.XmlStanzas
open Yaaf.Xmpp.IM.DelayedDelivery

type ForwardPayload =
    | MessageForward of MessageStanza
    | PresenceForward of PresenceStanza
    | IqForward of Stanza with
    member x.AsInterface =
        match x with
        | MessageForward msg -> msg.AsInterface
        | PresenceForward msg -> msg.AsInterface
        | IqForward msg -> msg.AsInterface
    member x.AsStanza = x.AsInterface.AsSimple

type ForwardInfo =
  { Inner : ForwardPayload
    Delay : DelayInfo option
    Xmlns : string } with
    member x.InnerMessage =
        x.Inner.AsInterface.As<MessageData>() : MessageStanza
    member x.InnerPresence =
        x.Inner.AsInterface.As<PresenceProcessingType>() : PresenceStanza
    member x.InnerStanza =
        x.Inner.AsStanza

module Parsing = 
    open System.Xml.Linq
    open Yaaf.Xml
    open Yaaf.Xmpp.XmlStanzas.Parsing

    let forwardNs = "urn:xmpp:forward:0"

    let tryFindForwardElem (children: XElement seq) = 
        children
            |> Seq.tryFind (fun e -> e.Name = getXName "forwarded" forwardNs)

    let tryGetForwardElem (stanza:Stanza<_>) =
        stanza.Contents.Children
        |> tryFindForwardElem
    
    let hasContentForward (stanza:Stanza<_>) = (tryGetForwardElem stanza).IsSome 
   
    let parseForwardItem (elem:XElement) =
        let payload = 
            elem.Elements()
                |> Seq.tryFind (fun e -> e.Name.LocalName = "message" || e.Name.LocalName = "presence" || e.Name.LocalName = "iq")
        let ns, innerStanza = 
            match payload with
            | None -> failwith "forwarded payload was not found"
            | Some inner ->
                let ns = inner.Name.Namespace.NamespaceName
                ns,
                match inner.Name.LocalName with
                | "message" -> MessageForward (Parsing.parseMessageStanza ns inner)
                | "presence" -> PresenceForward (Parsing.parsePresenceStanza ns inner)
                | "iq" -> IqForward (Parsing.parseStanzaElementNoError ns inner)
                | _ -> failwith "unknown forwarded payload"
        { Inner = innerStanza
          Delay = Parsing.tryFindDelayItem (elem.Elements())
          Xmlns = ns }
          
    let tryFindForwardItem (children: XElement seq) = 
        children
            |> tryFindForwardElem
            |> Option.map parseForwardItem

    let tryGetForwardItem (stanza:IStanza) =
        stanza.Contents.Children
        |> tryFindForwardItem

    let createForwardedInfoElem (forward:ForwardInfo) = 
        [
            if forward.Delay.IsSome then
                yield Parsing.createDelayInfoElem forward.Delay.Value :> obj
            yield Parsing.createStanzaElement forward.Xmlns forward.Inner.AsInterface :> obj
        ] |> getXElemWithChilds (getXName "forwarded" forwardNs)
    
    let addForwardedInfoRaw (forward:ForwardInfo) (stanza: Stanza) =
        stanza.AddContent(createForwardedInfoElem forward)
   
    let addForwardedInfo (forward:ForwardInfo) (stanza: Stanza<_>) =
        stanza.AddContent(createForwardedInfoElem forward)
