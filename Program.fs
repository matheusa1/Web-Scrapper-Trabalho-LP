open FSharp.Data

type URL = HtmlProvider<"https://www.amazon.com.br/s?rh=n%3A20971488011&language=pt_BR&brr=1&pf_rd_i=7791985011&pf_rd_m=A3RN7G7QC5MWSZ&pf_rd_p=312992ad-6965-4e90-9ec7-00ad3586c280&pf_rd_r=TBQ1WPG4F1PHB05KMNSG&pf_rd_s=merchandised-search-1&pf_rd_t=101&rd=1">
let f1Url = "https://pt.wikipedia.org/wiki/Campeonato_Mundial_de_F%C3%B3rmula_1_de_2023"
type WikiURL = HtmlProvider<"https://pt.wikipedia.org/wiki/Campeonato_Mundial_de_F%C3%B3rmula_1_de_2023">
let getData(url: string) =
  let page = HtmlDocument.Load(url)
  let temperatura = 
    page.CssSelect(".dato-temperatura") |> Seq.head |> (fun x -> x.InnerText())

  let descricao =
    page.CssSelect(".descripcion strong") |> Seq.head |> (fun x -> x.InnerText())

  let cidade =
    page.CssSelect(".titulo") |> Seq.head |> (fun x -> x.InnerText())
  (cidade, temperatura, descricao)
let getCards (page: HtmlDocument) = 
    page.Descendants["span"]
    |> Seq.filter(fun x ->
        match x.TryGetAttribute("class") with
        | None -> false
        | Some att ->
            match att.Value() with
            | "a-size-medium a-color-base a-text-normal" -> true
            | "a-price-whole" -> true
            | "a-price-fraction" -> true
            | _ -> false
    )
    |> Seq.map (fun t ->
        let text = 
            t.InnerText()
        text
    )  
let printCards(list: string list) =
    let rec print(seg) =
        match seg with
        | [] -> ()
        | _ ->
            let group, rest = seg |> List.splitAt 3
            printfn "Nome:%s\nPreco:%s%s\n" group[0] group[1] group[2]
            print rest
    print(list)

let g1GetTitle (url: string) = 
  let page = HtmlDocument.Load(url)
  page.Descendants["h1"] |> Seq.filter(fun x -> 
    match x.TryGetAttribute("class") with
    | None -> false
    | Some att ->
      match att.Value() with
      | "content-head__title" -> true
      | _ -> false
  ) 
  |> Seq.map (fun c -> 
    let titleText = c.InnerText()
    titleText
  )

let g1GetContent (url: string) =
  let page = HtmlDocument.Load(url)
  page.Descendants["p"] |> Seq.filter(fun x -> 
    match x.TryGetAttribute("class") with
    | None -> false
    | Some att ->
      match att.Value() with
      | "content-text__container " -> true
      | _ -> false
  )
  |> Seq.map (fun c -> 
    let contentText = c.InnerText()
    contentText
  )

let getWikiTable () =
  let f1Calendar = WikiURL().Tables.``Por Grande PrêmioEditar``

  for row in f1Calendar.Rows do
    printfn "\nGrande Premio: %s\nVencedor: %s\n" row.``Grande Prêmio`` row.Vencedor
let wrapper (args:string[]) =
  if args.Length < 1 then
    printfn "Usage: dotnet run <type>"
    exit 1

  if args.[0] = "help" then
    printfn "\ncommands:\n\n dotnet run amazon\n dotnet run g1 <url>\n dotnet run clima <city>\n dotnet run wiki\n"
    exit 0
  
  if args.[0] = "amazon" then
    let cards = getCards(URL().Html) |> Seq.toList
    printCards(cards)
    exit 0

  if args.[0] = "g1" then
    if args.Length < 2 then
      printfn "Usage: dotnet run g1 <url>"
      exit 1

    let url = args.[1]
    let title = g1GetTitle(url) 
    let content = g1GetContent(url)
    title |> Seq.toList |> List.map (fun x -> printfn "%s" x) |> ignore
    printfn "\n"
    content |> Seq.toList |> List.map (fun x -> printfn "%s" x) |> ignore
    exit 0
    
  if args[0] = "wiki" then
    getWikiTable() |> ignore
    exit 0


  if args.Length < 2 then
    printfn "Usage: dotnet run clima <city>"
    exit 1

  let param = args.[1]

  let url = "https://www.tempo.com/" + param + ".htm"

  getData(url) |> (fun (cidade, temperatura, descricao) ->
    printfn "%s" cidade
    printfn "Temperatura: %s" temperatura
    printfn "Descrição: %s" descricao
)

[<EntryPoint>]
let main argv =
    wrapper argv
    0