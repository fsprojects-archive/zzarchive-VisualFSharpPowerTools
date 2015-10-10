(*** hide ***)
// This block of code is omitted in the generated HTML documentation. Use 
// it to define helpers that you do not want to show in the documentation.
#I "../../bin"

(**
Lint
-----------------------

An [F# linter](http://fsprojects.github.io/FSharpLint/) runs as you modify your source code to try to highlight code that could possibly be improved. The linter can be turned off from the F# Power Tools general settings where it is listed as `FSharpLint Integration`.

### Rules

When a broken lint rule is discovered the first two characters of the code in question are underlined in orange, a mouse-over of these characters will show the lint's suggestion in a tooltip:

![Example of a broken lint rule](img/lint_rule_warning.gif)

Each rule can be turned off in the F# Power Tools lint settings under the rules tab, the checkboxes indicate whether a rule is enabled/disabled. Some rules are grouped in a collection under an analyser, disabling the analyser will disable all of its rules, to enable/disable specific rules expand the analyser to view its rules:

![Example of disabling a lint rule](img/lint_disable_rule.gif)

Some rules and analysers have additional settings, these can be viewed and updated by selecting the rule/analyser:

![Example of changing a rule's settings](img/lint_change_rule_setting.gif)

### Hints

### Configuration Files



*)