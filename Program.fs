module Fautology
open Expecto
open FParsec

type Predicate =
    | Const of bool
    | And of Predicate * Predicate
    | Or of Predicate * Predicate
    | Not of Predicate
    | Imply of Predicate * Predicate

let str_ws s = pstring s >>. spaces

let opp = new OperatorPrecedenceParser<Predicate, unit, unit>()
let expr = opp.ExpressionParser
opp.TermParser <- ((stringReturn "T" (Const true)) <|> (stringReturn "F" (Const false))) <|> between (str_ws "(") (str_ws ")") expr

opp.AddOperator(InfixOperator("^", spaces, 2, Associativity.Left, fun p q -> And (p, q)))
opp.AddOperator(InfixOperator("v", spaces, 2, Associativity.Left, fun p q -> Or (p, q)))
opp.AddOperator(InfixOperator("=>", spaces, 4, Associativity.Left, fun p q -> Imply (p, q)))
opp.AddOperator(PrefixOperator("~", spaces, 3, false, Not))

let rec evalPredicate = function
| (Const p) -> p
| And (p, q) -> (evalPredicate p) && (evalPredicate q)
| Or (p, q) -> (evalPredicate p) || (evalPredicate q)
| Not p -> not (evalPredicate p)
| Imply (p, q) -> (evalPredicate p) <= (evalPredicate q)

let completeExpression = spaces >>. expr .>> eof

let parsePredicate s =
    match run completeExpression s with
    | Success (predicate, _, _) ->
        printfn "%A" predicate
        predicate
    | Failure (errorMsg, _, _) ->
        failwith errorMsg


let tests =
    let predicates = [
        ("T", true); ("F", false); ("~T", false); ("~F", true); ("T^T", true); ("T^F", false); ("T^~T", false);
        ("Fv~F", true); ("~F^~F", true); ("F=>~T", true); ("(~T^~T)=>F", true); ("(TvF)=>T^(~T^~F)=>F", true)
    ]
    testList "predicate parsing" [
        for (predicate, expected) in predicates do
        yield testCase (sprintf "%A is %A" predicate expected) <| fun _ ->
            Expect.equal (parsePredicate predicate |> evalPredicate) expected (sprintf "%A was not %A" predicate expected)
    ]

[<EntryPoint>]
let main argv =
    runTestsWithArgs defaultConfig [|"--sequenced"; "--debug"|] tests
