namespace FunctionalShoppingCart

module Amount =
    type _T = Amount of int
    let create (a: int) =
        if a > 0
        then Some (Amount a)
        else None
    let value (Amount m) = m
    
type Currency = EUR | USD | DKK | GBP

module Price =
    type _T = {
        Amount: Amount._T
        Currency: Currency
    }
    let create (amount: int, curr: Currency) =
        let priceAmount = Amount.create amount
        match priceAmount with
        | Some priceAmount -> Some { Amount = priceAmount; Currency = curr }
        | _ -> None

module CardExpirationMonth =
    type _T = CardExpirationMonth of int
    let create (month: int) =
        if month > 0 && month <= 12
            then Some (CardExpirationMonth month)
            else None
    let apply f (CardExpirationMonth m) = f m
    let value m = apply id m
    
module CardExpirationYear =
    type _T = CardExpirationYear of int
    let create (y: int) =
        let currentYear = System.DateTime.Now.Year
        if y >= currentYear && y < currentYear + 10
            then Some (CardExpirationYear y)
            else None
    let apply f (CardExpirationYear y) = f y
    let value y = apply id y 

module CardExpirationDate =
    type _T = {
        Month: CardExpirationMonth._T
        Year: CardExpirationYear._T
    }
    let create (m: int, y: int) =
        let expMonth =  CardExpirationMonth.create m
        let expYear = CardExpirationYear.create y
        match expMonth, expYear with
        | Some expMonth, Some expYear ->
            Some { Month = expMonth; Year = expYear }
        | _ -> None
        
module CardNumber =
    type _T = CardNumber of int
    let rec digitCount number = if number < 10 then 1 else 1 + digitCount (number / 10)
    let _digitsCount = 16
    let create (n: int) =
        if digitCount n = _digitsCount
        then Some (CardNumber n)
        else None
    let apply f (CardNumber n) = f n
    let value n = apply id n

type CardIssuer = Visa | MasterCard | Maestro | Dankort

module CreditCard =
    type _T = {
        Number: CardNumber._T
        Issuer: CardIssuer
        ExpirationDate: CardExpirationDate._T
    }
    let create (number: int) (issuer: CardIssuer) (exp: int * int) =   
        let cardNum = CardNumber.create number
        let cardExp = CardExpirationDate.create exp
        match cardNum, cardExp with
        | Some cardNum, Some cardExp -> Some { Number = cardNum; Issuer = issuer; ExpirationDate = cardExp }
        | _ -> None
        