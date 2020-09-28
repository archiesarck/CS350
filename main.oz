functor
import
    Integer
    Floats
    System
    Browser
    Application
define

    % Question 1.1
    fun {Take Xs N}
        if N=<0 then nil
        else
            case Xs
            of nil then nil
            [] H|T then H | {Take T N-1}
            end
        end
    end

    % Question 1.2
    fun {Last Xs N}
        local
            fun {Reverse Ys Partial}
                case Ys
                of nil then Partial
                [] H|T then {Reverse T H|Partial}
                end
            end
        in
        {Reverse {Take {Reverse Xs nil} N} nil}
        end
    end

    % Question 1.3
    fun {Merge Xs Ys}
        local
            fun {Merger L1 L2 Merged}
                case L1
                of nil then
                    case L2
                    of nil then Merged
                    else {Append Merged L2}
                    end
                [] H|T then
                    case L2
                    of nil then {Append Merged L1}
                    [] A|B then
                        if H=<A then {Merger T L2 {Append Merged [H]}}
                        else {Merger L1 B {Append Merged [A]}}
                        end
                    end
                end
            end
        in
        {Merger Xs Ys nil}
        end
    end

    % Question 2.1
    fun {Add N1 N2}
        N1+N2
    end
    fun {ZipWith BinOp Xs Ys}
        local
            fun {Zipper L1 L2 Partial}
                case L1
                of nil then
                    case L2
                    of nil then Partial
                    else {Append Partial L2}
                    end
                [] H|T then
                    case L2
                    of nil then {Append Partial L1}
                    [] A|B then {Zipper T B {Append Partial [{BinOp H A}]}}
                    end
                end
            end
        in
        {Zipper Xs Ys nil}
        end
    end


    % Question 2.2
    fun {Doubler N}
        2*N
    end
    fun {Map Xs F}
        local
            fun {MyAppend N Zs}
                {Append [{F N}] Zs}
            end
        in
        {FoldR Xs MyAppend nil}
        end
    end


    % Question 2.3
    fun {FoldL B Xs I}
        local
            fun {Reverse Remainder Partial}
                case Remainder
                of nil then Partial
                [] H|T then {Reverse T H|Partial}
                end
            end
            fun {FoldLAux B Ys I}
                case Ys
                of nil then I
                [] H|T then {B {FoldLAux B T I} H}
                end
            end
        in
        {FoldLAux B {Reverse Xs nil} I}
        end
    end

    % Question 3.1
    % Non-optimized approach, calculating factorial and power every time

    % fun {Taylor X}
    %     local
    %         fun {Factorial N Product}
    %             if N==0 then Product
    %             else {Factorial N-1 N*Product}
    %             end
    %         end
    %         fun {Power Num Exp Product}
    %             % {System.showInfo Product}
    %             if Exp==0 then Product
    %             else {Power Num Exp-1 Num*Product}
    %             end
    %         end
    %         fun lazy {Series N Sign}
    %             ({Power ~1.0 Sign 1.}*({Power X N 1.}/{IntToFloat {Factorial N 1}}))|{Series N+2 Sign+1}
    %         end
    %     in
    %     {Series 1 0}
    %     end
    % end

    % using some kind of dynamic programming
    fun {Taylor X}
        local
            fun lazy {Series Sign Power Fact N}
                (Sign*Power/{IntToFloat Fact})|{Series ~Sign Power*X*X Fact*(2*N+2)*(2*N+3) N+1}
            end
        in
        {Series 1. X 1 0}
        end
    end

    fun {Sine X N}
        local
            fun {Addup Xs S N}
                if N==0 then S
                else
                    case Xs of H|T then {Addup T H+S N-1} end
                end
            end
        in
        {Addup {Taylor X} 0. N}
        end
    end
    {System.showInfo {Sine 0.523598775598298873 1000}}


    % Question 3.2
    fun {Approximate S Epsilon}
        local
            fun {Addup Xs F}
                case Xs of H|B|T then
                    if {Abs H-B}>Epsilon then {Addup B|T H+F}
                    else F+H
                    end
                end
            end
        in
        {Addup S 0.}
        end
    end
    {System.showInfo {Approximate {Taylor 0.52} 0.000001}}

    % Question 4

    fun {IsDiagonal M}
        local
            fun {CheckRow Rs Row Current}
                case Rs
                of nil then true
                [] H|T then
                    if Current==Row then {CheckRow T Row Current+1}
                    else
                        if H==0 then {CheckRow T Row Current+1}
                        else false
                        end
                    end
                end
            end

            fun {Check M Row}
                case M
                of nil then true
                [] U|D then
                    if {CheckRow U Row 1}==false then false
                    else {Check D Row+1}
                    end
                end
            end
        in
        {Check M 1}
        end
    end
    {System.print {IsDiagonal [[1 0 0] [0 1 0] [0 0 3]]}}
end