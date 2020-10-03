functor
import
    Integer
    System
    Browser
    Application
define
    fun {Degree P Q}
        case P of H|T then
            case Q of A|B then H.2 + A.2 + 1
            else 0
            end
        else 0
        end
    end

    fun {Mono Term Q Prod N}
        if N==0 then Prod
        else
            case Q of H|T then
                if Term.2 + H.2 + 1 == N then
                    Prod.N = (H.1) * (Term.1)
                    {Mono Term T Prod N-1}
                else
                    Prod.N = 0
                    {Mono Term H|T Prod N-1}
                end
            else
                Prod.N = 0
                {Mono Term nil Prod N-1}
            end
        end
    end

    fun {Product P Q}
        local
            fun {Zeroes N Zs}
                if N==0 then Zs
                else {Zeroes N-1 {Append Zs [0]}}
                end
            end
            fun {Add A B C N}
                case A of nil then C
                [] H|Hs then {Add Hs B {Append C [H + B.N]} N+1}
                end
            end
            fun {Multiply D P Q R}
                case P of nil then R
                [] H|T then {Multiply D T Q {Add R {Mono H Q {Tuple.make polynomial D} D} nil 1}}
                end
            end
        in
        {Multiply {Degree P Q} P Q {Zeroes {Degree P Q} nil}}
        end
    end
    
    P = [term(1:2 2:1) term(1:1 2:0)]
    Q = [term(1:2 2:1) term(1:1 2:0)]
    {System.print {Product P Q}}
end