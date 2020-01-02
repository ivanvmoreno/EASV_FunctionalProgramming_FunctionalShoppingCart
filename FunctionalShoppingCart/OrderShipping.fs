namespace FunctionalShoppingCart

type ShippingMethods = Standard | Express

module OrderShipping =
    type _T = {
        ShippingMethod: ShippingMethods
        ShippingPrice: Price._T
    }