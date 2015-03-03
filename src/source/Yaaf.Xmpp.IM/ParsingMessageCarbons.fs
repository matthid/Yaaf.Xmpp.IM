// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
/// parsing logic for XEP-0280: Message Carbons (http://xmpp.org/extensions/xep-0280.html)
namespace Yaaf.Xmpp.IM.MessageCarbons

open System
open Yaaf.Xmpp
open Yaaf.Xmpp.IM
open Yaaf.Xmpp.XmlStanzas
open Yaaf.Xmpp.IM.DelayedDelivery
open Yaaf.Xmpp.IM.StanzaForwarding

type CarbonType =
    | Received
    | Sent

type CarbonInfo =
  { Forwarded : ForwardInfo
    ForwardType : CarbonType } with
    member x.InnerMessage =
        x.Forwarded.InnerMessage

type MessageCarbonPayload = 
    | Private
    
type CarbonIqAction =
    | Enable
    | Disable

type CarbonIqStanza = Stanza<CarbonIqAction>

module Parsing = 
    open System.Xml.Linq
    open Yaaf.Xml
    open Yaaf.Xmpp.XmlStanzas.Parsing
    
    let carbonNs = "urn:xmpp:carbons:2"
    open Yaaf.Xmpp.XmlStanzas.StanzaParseException
    open Yaaf.Helper

    // Iq stanzas
    let isContentIqCarbon (stanza:IStanza) = 
        if stanza.Header.StanzaType <> XmlStanzaType.Iq  then false
        else
        let queryItem =
            stanza.Contents.Children 
            |> Seq.tryFind (fun e -> e.Name = getXName "enable" carbonNs || e.Name = getXName "disable" carbonNs)
        queryItem.IsSome 

    let parseContentIqCarbonElem (elem:XElement) = 
        if elem.Name.NamespaceName <> carbonNs then elemFail elem "invalid IQ Carbon element!"
        match elem.Name.LocalName with
        | "enable" -> CarbonIqAction.Enable
        | "disable" -> CarbonIqAction.Disable
        | _ -> elemFail elem "invalid IQ Carbon element!"

    let parseContentIqCarbon (stanza:IStanza) =
        if stanza.Header.StanzaType <> XmlStanzaType.Iq then parseFail stanza "expected iq stanza"
        else
            stanza.Contents.Children 
            |> Seq.filter (fun e -> e.Name = getXName "enable" carbonNs || e.Name = getXName "disable" carbonNs)
            |> Seq.map parseContentIqCarbonElem
            |> Seq.exactlyOneExn (lazy parseExn stanza "")
    

    let parseIqCarbonStanza ns (elem:XElement) =  // parse something within the "stream"
        parseGenericStanza ns parseContentIqCarbon elem

    let createIqCarbonStanzaElement (action: CarbonIqAction) = 
        let elemName =
            match action with
            | CarbonIqAction.Disable -> "disable"
            | CarbonIqAction.Enable -> "enable"
        [
        ] |> getXElemWithChilds (getXName elemName carbonNs)

    let iqCarbonContentGenerator = ContentGenerator.SimpleGenerator createIqCarbonStanzaElement
    let createIqCarbonElement (id:string) (fromJid:JabberId option) (data:CarbonIqAction) = 
        Stanza<_>.CreateGen iqCarbonContentGenerator
          { To = None // always sent to own server
            From = fromJid
            Id = Some id
            Type = Some "set"
            StanzaType = XmlStanzaType.Iq }
          data

    // Message payloads

    // Carbon payload

    let tryFindCarbonElem (children: XElement seq) = 
        children
            |> Seq.tryFind (fun e -> e.Name = getXName "sent" carbonNs || e.Name = getXName "received" carbonNs)

    let tryGetCarbonElem (stanza:Stanza<_>) =
        stanza.Contents.Children
        |> tryFindCarbonElem
    
    let hasContentCarbon (stanza:Stanza<_>) = (tryGetCarbonElem stanza).IsSome 
   
    let parseCarbonItem (elem:XElement) =
        let payload = 
            elem.Elements()
                |> Seq.tryFind (fun e -> e.Name = getXName "forwarded" Parsing.forwardNs)
        let payload = 
            match payload with
            | None -> failwith "carbon payload was not found"
            | Some inner ->
                Parsing.parseForwardItem inner
        if elem.Name.NamespaceName <> carbonNs then elemFail elem "unknown carbon namespace"
        let payloadType =
            match elem.Name.LocalName with
            | "sent" -> CarbonType.Sent
            | "received" -> CarbonType.Received
            | _ ->  elemFail elem "unknown carbon item"
        { Forwarded = payload
          ForwardType = payloadType } 
          
    let tryFindCarbonItem (children: XElement seq) = 
        children
            |> tryFindCarbonElem
            |> Option.map parseCarbonItem

    let tryGetCarbonItem (stanza:IStanza) =
        stanza.Contents.Children
        |> tryFindCarbonItem

    let createCarbonInfoElem (carbon:CarbonInfo) = 
        let name =
            match carbon.ForwardType with
            | CarbonType.Sent -> "sent"
            | CarbonType.Received -> "received"
        [
            yield Parsing.createForwardedInfoElem carbon.Forwarded
        ] |> getXElemWithChilds (getXName name carbonNs)
    
    let addCarbonInfoRaw (carbon:CarbonInfo) (stanza: Stanza) =
        stanza.AddContent(createCarbonInfoElem carbon)
   
    let addCarbonInfo (carbon:CarbonInfo) (stanza: Stanza<_>) =
        stanza.AddContent(createCarbonInfoElem carbon)


    // private element
    
    let tryFindCarbonPrivateElem (children: XElement seq) = 
        children
            |> Seq.tryFind (fun e -> e.Name = getXName "private" carbonNs)

    let tryGetCarbonPrivateElem (stanza:Stanza<_>) =
        stanza.Contents.Children
        |> tryFindCarbonElem
    
    let hasContentCarbonPrivate (stanza:Stanza<_>) = (tryGetCarbonElem stanza).IsSome 
   
    let parseCarbonPrivateItem (elem:XElement) =
        if elem.Name.NamespaceName <> carbonNs then elemFail elem "unknown carbon private  namespace"
        if elem.Name.LocalName <> "private" then elemFail elem "unknown carbon private name"
        MessageCarbonPayload.Private
          
    let tryFindCarbonPrivateItem (children: XElement seq) = 
        children
            |> tryFindCarbonPrivateElem
            |> Option.map parseCarbonPrivateItem

    let tryGetCarbonPrivateItem (stanza:IStanza) =
        stanza.Contents.Children
        |> tryFindCarbonPrivateItem

    let createCarbonPrivateElem (priv:MessageCarbonPayload) = 
        [
        ] |> getXElemWithChilds (getXName "private" carbonNs)
    
    let addCarbonPrivateRaw (priv:MessageCarbonPayload) (stanza: Stanza) =
        stanza.AddContent(createCarbonPrivateElem priv)
   
    let addCarbonPrivateInfo (priv:MessageCarbonPayload) (stanza: Stanza<_>) =
        stanza.AddContent(createCarbonPrivateElem priv)