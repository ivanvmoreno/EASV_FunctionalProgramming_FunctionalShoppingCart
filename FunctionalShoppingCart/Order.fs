namespace FunctionalShoppingCart

module ItemQuantity =
    type _T = ItemQuantity of int
    let create (q: int) =
        if q > 0 && q < 99
        then Some (ItemQuantity q)
        else None
    let value (ItemQuantity q) = q

module OrderItem =
    type _T = {
        Price: Price._T
        Name: string
        Quantity: ItemQuantity._T
    }
    let create (amount: int, curr: Currency) (name: string) (q: int) =
        let itemPrice = Price.create (amount, curr)
        let itemQuantity = ItemQuantity.create q
        match itemPrice, itemQuantity with
        | Some itemPrice, Some itemQuantity -> Some { Price = itemPrice; Name = name; Quantity = itemQuantity }
        | _ -> None

type OrderStatus = Cancelled | Delivered | Paid | PendingPayment | Sent

type OrderPaymentMethod = CreditCard

module Order =
    type _T = {
        Customer: Customer._T
        Items: List<OrderItem._T>
        PaymentMethod: OrderPaymentMethod
        ShippingMethod: OrderShipping._T
        State: OrderStatus
    }