// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Yaaf.Xmpp.Runtime.Features
(* 
    XMPP.IM 2.6.1.  Stream Feature
    If a server supports roster versioning, then it MUST advertise the following stream feature during stream negotiation.
     <ver xmlns='urn:xmpp:features:rosterver'/>
    The roster versioning stream feature is merely informative and therefore is never mandatory-to-negotiate. 
*)
open FSharpx.Collections
open Yaaf.Logging
open Yaaf.Helper // required by Equals patterm matching
// Note that we depend on XmlStanzas to be present

module ParsingRosterVer =
    open System.Xml.Linq
    open Yaaf.Xml
    let rosterVerNs = "urn:xmpp:features:rosterver"
    // element within "feature" advertisement
    let checkIfRosterVer (elem:XElement) = 
        match elem.Name.NamespaceName, elem.Name.LocalName with
        | (Equals rosterVerNs), "ver" -> true
        | _ -> false

    let createAnnouncementElement () = 
        getXName "ver" rosterVerNs |> getXElem

open ParsingRosterVer
type RosterVerFeature (sasl : ISaslService) = 
    interface IStreamFeatureHandler with 
        member __.PluginService = Seq.empty
        member __.GetState (featureList) = 
            if not sasl.IsRemoteAuthenticated then
                Unavailable
            else
                let maybeFound =
                    featureList 
                    |> Seq.choose(fun elem -> if checkIfRosterVer elem then Some elem else None)
                    |> Seq.tryHead
                match maybeFound with
                | None -> 
                    //context.Permanent.SessionAvailable <- false
                    Unavailable
                | Some (elem) -> 
                    FeatureInfo.NegotiatedOrAnnounceOnly elem

        /// Used when client selects feature
        member __.InitializeFeature () =
            async {
                // Must not be selected!
                failwith "NegotiatedOrAnnounceOnly featues can't be selected!"
                return ()
            } |> Log.TraceMe

    // Server
        // Features can disable itself by returning None
        member x.CreateAnnounceFeatureElement () = 
            if not sasl.IsRemoteAuthenticated then
                FeatureInfo.Unavailable
            else
                // announce only
                FeatureInfo.NegotiatedOrAnnounceOnly (createAnnouncementElement ())

        /// Used so that the receiving entity can select the selected feature
        member __.IsFeatureSelected elem = false // isRosterVerElement config.StreamNamespace elem
        member __.HandleReceivingCommunication (elem) =
            async {
                failwith "NegotiatedOrAnnounceOnly featues can't be selected!"
                return ()
            } 
            |> Log.TraceMe
