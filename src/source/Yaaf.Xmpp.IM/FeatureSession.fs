// ----------------------------------------------------------------------------
// This file is subject to the terms and conditions defined in
// file 'LICENSE.txt', which is part of this source code package.
// ----------------------------------------------------------------------------
namespace Yaaf.Xmpp.Runtime.Features
(*
    This implementation is obsolete since rfc6121
*)
open FSharpx.Collections
open Yaaf.Logging
open Yaaf.Helper
// Note that we depend on XmlStanzas to be present
open Yaaf.Xmpp.Runtime
open Yaaf.Xmpp.XmlStanzas

type SessionElement = 
    /// Request generated resource
    | Request
    | SuccessResponse

module ParsingFeatureSession =
    open System.Xml.Linq
    open Yaaf.Xml
    open Yaaf.Xmpp.XmlStanzas.Parsing
    let sessionNs = "urn:ietf:params:xml:ns:xmpp-session"
    // Check if this is a session request -> iq stanza
    let isContentSession (stanza:IStanza) = 
        if stanza.Header.StanzaType <> XmlStanzaType.Iq || stanza.Contents.Children |> List.length > 1 then false
        else
        let maybeChild = stanza.Contents.Children |> Seq.tryHead
        match maybeChild with
        | Some child ->
            match child.Name.NamespaceName, child.Name.LocalName with
            | Equals sessionNs, "session" -> true
            | _ -> false
        | None -> stanza.Header.Type.IsSome && stanza.Header.Type.Value = "result"
    let isSessionElement ns (elem:XElement) = isGenericStanza ns isContentSession elem

    // Stream elements
    let parseContentSession (stanza:IStanza) =
        if stanza.Header.StanzaType <> XmlStanzaType.Iq || stanza.Contents.Children |> List.length > 1 then failwith "expected session element"
        else
        let maybeChild = stanza.Contents.Children |> Seq.tryHead
        match maybeChild with
        | Some child ->
            match child.Name.NamespaceName, child.Name.LocalName with
            | Equals sessionNs, "session" -> 
                SessionElement.Request
            | _ -> failwith "expected session element"
        | None -> 
            if stanza.Header.Type.IsSome && stanza.Header.Type.Value = "result" then
                SessionElement.SuccessResponse
            else failwith "can't be a session stanza"

    let parseSessionCommand ns (elem:XElement) =  // parse something within the "stream"
        parseGenericStanza ns parseContentSession elem

    let createSessionContentElement (command:SessionElement) = 
        match command with
        | Request -> [ (getXName "session" sessionNs) |> getXElem ]
        | SuccessResponse -> []
    let sessionContentGenerator = ContentGenerator.NormalGenerator createSessionContentElement
    let createSessionElement id (command:SessionElement) = 
        let cType = 
            match command with
            | SuccessResponse _ -> "result"
            | Request -> "set"
        Stanza<_>.CreateGen sessionContentGenerator
          { To = None
            From = None
            Id = Some (id)
            Type = Some (cType)
            StanzaType = XmlStanzaType.Iq }
          command 

    // element within "feature" advertisement
    let checkIfSession (elem:XElement) = 
        match elem.Name.NamespaceName, elem.Name.LocalName with
        | (Equals sessionNs), "session" -> Some <| ()
        | _ -> None

    let createAnnouncementElement () = 
        getXName "session" sessionNs |> getXElem


open ParsingFeatureSession
type SessionFeature (sasl : ISaslService, runtimeConfig : IRuntimeConfig, bind : IBindService, coreApi : ICoreStreamApi) = 
    let mutable activeSession = false
    let xmlStream = coreApi.AbstractStream
    let writeElem s =
        xmlStream.Write (Parsing.createStanzaElement runtimeConfig.StreamNamespace s)
    let readElem () =
        async {
            let! read = xmlStream.ReadElement()
            return ParsingFeatureSession.parseSessionCommand runtimeConfig.StreamNamespace read
        }
    
    interface IStreamFeatureHandler with 
        member __.PluginService = Seq.empty
        member __.GetState (featureList) = 
            if not sasl.IsRemoteAuthenticated || activeSession then
                Unavailable
            else
                let maybeFound =
                    featureList 
                    |> Seq.choose(fun elem -> checkIfSession elem |> Option.map (fun t -> elem, t))
                    |> Seq.tryHead
                match maybeFound with
                | None -> 
                    //context.Permanent.SessionAvailable <- false
                    Unavailable
                | Some (elem, _) -> 
                    // no-op since rfc6121 
                    //context.ActiveSession <- true
                    FeatureInfo.NegotiatedOrAnnounceOnly elem
                    //Available (false, elem)

        /// Used when client selects feature
        member __.InitializeFeature () =
            async {
                Log.Verb(fun _ -> "starting session request")
                //do! context.XmppStream.WriteStanza()
                let sentId = 
                    System.Guid.NewGuid().ToString()
                    //context.GenerateNextId()
                do! writeElem (createSessionElement sentId SessionElement.Request)
                let! result = readElem ()
                if result.Header.Id.IsNone || sentId <> result.Header.Id.Value then
                    failwith "expected the result to have the same id"
                match result.Data with
                | SessionElement.SuccessResponse -> 
                    activeSession <- true
                | _ -> failwith "expected SuccessResponse from server"
                //failwith "not implemented"
                Log.Verb(fun _ -> "finished session request")
                return ()
            } |> Log.TraceMe

    // Server
        // Features can disable itself by returning None
        member __.CreateAnnounceFeatureElement () = 
            if not sasl.IsRemoteAuthenticated || activeSession then
                FeatureInfo.Unavailable
            else
                // not mandatory since rfc6121 
                //context.ActiveSession <- true
                //FeatureInfo.NegotiatedOrAnnounceOnly (createAnnouncementElement ())
                FeatureInfo.Available (false, createAnnouncementElement ())
        /// Used so that the receiving entity can select the selected feature
        member __.IsFeatureSelected elem = isSessionElement runtimeConfig.StreamNamespace elem
        member __.HandleReceivingCommunication (elem) =
            async {
            if bind.BoundJid.IsNone then
                do! coreApi.FailwithStream (new StreamErrorException(XmlStreamError.NotAuthorized, None, []))
                return failwith "Use session feature after resource binding"
            else
                let sessionCommand = parseSessionCommand runtimeConfig.StreamNamespace elem
                match sessionCommand.Data with
                | SessionElement.Request ->
                    do! writeElem (createSessionElement sessionCommand.Header.Id.Value SessionElement.SuccessResponse)
                    activeSession <- true
                | _ -> failwith "expected Request from client"

                return ()
            } |> Log.TraceMe

