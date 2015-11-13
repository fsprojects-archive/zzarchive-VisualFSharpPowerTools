module FSharpVSPowerTools.GuiUtils

open FsXaml
open FSharp.ViewModule
open System


/// Add a quoted dependency to the viewmodel base
let addDependency (vmBase:ViewModelBase) (prop1:Quotations.Expr) (prop2:Quotations.Expr) =
    vmBase.DependencyTracker.AddPropertyDependency(prop1,prop2)

/// Add a list of quoted dependencies to the viewmodel base
let addDependencies (vmBase:ViewModelBase) (prop1:Quotations.Expr) (props:Quotations.Expr list) =
    vmBase.DependencyTracker.AddPropertyDependencies(prop1,props)

/// Add a command to a quoted property of the viewmodel base
let addCommand (vmBase:ViewModelBase) (cmd:INotifyCommand) (prop:Quotations.Expr) =
    vmBase.DependencyTracker.AddCommandDependency(cmd,prop)

let inline setBacking (vmbase:#ViewModelBase) arg1 arg2   = vmbase.Factory.Backing (arg1, arg2)
let inline makeCommand (vmbase:#ViewModelBase) func = vmbase.Factory.CommandSync func
let inline makeCommandParam (vmbase:#ViewModelBase) func = vmbase.Factory.CommandSyncParam func

