namespace FunctionalShoppingCart

module CustomerName =
    type _T = {
        First: string
        Middle: string option
        Last: string
    }
    let fullNameConcat ({First = fName; Middle = midName; Last = lName}) =
        let names =
            match midName with
            | Some midName -> [|fName; midName; lName|]
            | None -> [|fName; lName|]
        System.String.Join(" ", names)

module Email =
    type _T = {
        Address: string
        Verified: bool 
    }
    
module ZipCode =
    type _T = ZipCode of string
    let create (code: string) =
        let isValid = 
            System.Text.RegularExpressions.Regex.IsMatch(code, @"^\d{5}$") 
        if isValid
        then Some (ZipCode code)
        else None

type CustomerContactMethod = Email | PhoneNumber | TwitterProfile

module Cities =
    type _City = City of string
    let _create (citiesList: List<_City>) (city: string) =
        if List.exists (fun (City c) -> c = city) citiesList
            then Some (City city)
            else None
    
    module SpanishCity =
        type _T = SpanishCity of _City
        let create (city: string) =
            let _cities =
                [City "Madrid"; City "Barcelona"; City "Bilbao"; City "Vigo"]
            match _create _cities city with
            | Some c -> Some (SpanishCity c)
            | None -> None
    
    module DanishCity =
        type _T = DanishCity of _City
        let create (city: string) =
            let _cities =
                [City "Copenhagen"; City "Aarhus"; City "Esbjerg"]
            match _create _cities city with
            | Some c -> Some (DanishCity c)
            | None -> None

module Country =
    type _T = Country of string
    let create (country: string) =
        let _countriesList =
            [Country "Spain"; Country "Denmark"]
        if List.exists (fun (Country c) -> c = country) _countriesList
        then Some (Country country)
        else None

module CustomerAddress =
    module SpanishAddress =
        type _T = {
            Address1: string
            Address2: string option
            City: Cities.SpanishCity._T
            ZipCode: ZipCode._T
            Country: Country._T
        }
        let create (addr1: string) (addr2: string option) (city: string) (zip: string) =
            let custCity = Cities.SpanishCity.create city
            let custZip = ZipCode.create zip
            let country = Country.create "Spain"
            match custCity, custZip, country with
            | Some custCity, Some custZip, Some country ->
                Some { Address1 = addr1; Address2 = addr2; City = custCity; ZipCode = custZip; Country = country }
            | _ -> None
            
    type _T = SpanishAddress._T

module Customer =
    type _T = {
        Name: CustomerName._T
        ContactMethod: CustomerContactMethod
        Address: CustomerAddress._T
    }
