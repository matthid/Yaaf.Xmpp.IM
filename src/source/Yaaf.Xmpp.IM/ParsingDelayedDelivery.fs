// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
/// parsing logic for XEP-0203: Delayed Delivery (http://xmpp.org/extensions/xep-0203.html)
namespace Yaaf.Xmpp.IM.DelayedDelivery

open System
open Yaaf.Xmpp
open Yaaf.Xmpp.XmlStanzas

type DelayInfo =
  { Reason : string option
    From : JabberId option
    Stamp : DateTime }
    
module Parsing = 
    open System.Xml.Linq
    open Yaaf.Xml
    open Yaaf.Xmpp.XmlStanzas.Parsing

    let delayNs = "urn:xmpp:delay"
    
    let tryFindDelayElem (children : XElement seq) = 
        children
            |> Seq.tryFind (fun e -> e.Name = getXName "delay" delayNs)

    let tryGetDelayElem (stanza : IStanza) =
        if stanza.Header.StanzaType = XmlStanzaType.Iq then 
            None
        else
        let delayItem =
            stanza.Contents.Children
            |> tryFindDelayElem
        delayItem

    let hasContentDelay (stanza:IStanza) = (tryGetDelayElem stanza).IsSome

    let parseDelayItem (elem:XElement) =
        { Reason = match elem.Value with v when String.IsNullOrWhiteSpace v -> None | v -> Some v
          From = elem |> tryXAttrValue (getXName "from" "") |> Option.map JabberId.Parse
          Stamp = elem |> forceAttrValue (getXName "stamp" "") |> StreamData.DateTime.Parse }

    let tryGetDelayItem (stanza : IStanza) = tryGetDelayElem stanza |> Option.map parseDelayItem
    let tryFindDelayItem (children : XElement seq) = children |> tryFindDelayElem |> Option.map parseDelayItem

    let createDelayInfoElem (delay:DelayInfo) = 
        [
            yield XAttribute(getXName "stamp" "", delay.Stamp |> StreamData.DateTime.ToString) :> obj
            if delay.From.IsSome then
                yield XAttribute(getXName "from" "", delay.From.Value.FullId) :> obj
            if delay.Reason.IsSome then
                yield delay.Reason.Value :> obj
        ] |> getXElemWithChilds (getXName "delay" delayNs)
    
    let addDelayInfoRaw (delay:DelayInfo) (stanza: Stanza) =
        stanza.AddContent(createDelayInfoElem delay)
    let addDelayInfo (delay:DelayInfo) (stanza: Stanza<_>) =
        stanza.AddContent(createDelayInfoElem delay)

    
