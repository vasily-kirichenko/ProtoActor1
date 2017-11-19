open Proto
open System
open System.Threading

type Hello = { Who: string }
type Fatal = Fatal
type Recoverable = Recoverable
exception RecoverableException
exception FatalException

type ChildActor() =
    interface IActor with
        member __.ReceiveAsync ctx =
            match ctx.Message with
            | :? Hello as r -> printfn "Hello %A" r.Who
            | :? Recoverable -> raise RecoverableException
            | :? Fatal -> raise FatalException
            | :? Started -> printfn "Started, initialize actor here"
            | :? Stopping -> printfn "Stopping, actor is about shut down"
            | :? Stopped -> printfn "Stopped, actor and it's children are stopped"
            | :? Restarting -> printfn "Restarting, actor is about restart"
            | _ -> printfn "Unexpected message %O" ctx.Message
            Actor.Done

type ParentActor() =
    interface IActor with
        member __.ReceiveAsync ctx =
            let child =
                ctx.Children
                |> Option.ofObj
                |> Option.bind Seq.tryHead
                |> Option.defaultWith (fun _ ->
                     Actor.FromProducer(fun _ -> upcast ChildActor()) |> ctx.Spawn)
    
            match ctx.Message with
            | :? Hello -> child.Tell ctx.Message
            | :? Recoverable -> child.Tell ctx.Message
            | :? Fatal -> child.Tell ctx.Message
            | :? Terminated as r -> printfn "Watched actor was Terminated, %O" r.Who
            | _ -> printfn "Unextected message %O" ctx.Message
        
            Actor.Done

module Decider =
    let decide (_: PID) (reason: exn) =
        match reason with
        | :? RecoverableException -> SupervisorDirective.Restart
        | :? FatalException -> SupervisorDirective.Stop
        | _ -> SupervisorDirective.Escalate

[<EntryPoint>]
let main _ = 
    let props = 
        Actor.FromProducer(fun _ -> upcast ParentActor())
             .WithChildSupervisorStrategy(OneForOneStrategy(Decider Decider.decide, 1, Nullable()))
    let actor = Actor.Spawn props
    actor.Tell { Who = "Alex" }
    actor.Tell Recoverable
    actor.Tell Fatal
    Thread.Sleep (TimeSpan.FromSeconds 5.)
    actor.Stop()
    Console.ReadLine() |> ignore
    0