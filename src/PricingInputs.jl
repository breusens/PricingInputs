module PricingInputs
export printsize,printinput,findNan,ValuationInputs,MarketInputs,TradeInputs,Split,Mean,TDVariable
# Write your package code here.
import Base: *,+,-,^,/,sqrt,sign,abs,max,min,getindex
import Statistics: mean
using Derivatives
using Statistics

struct ValuationInputs{T1,T2}
    t::T1
    Maturity::T1
    Observations::T2
    TradeState::T2
    TradeParameters::T1
end


struct TradeInputs{T1,T2}
    Maturity::T1
    TradeState::T2
    TradeParameters::T1
end

struct MarketInputs{T1,T2}
    t::T1
    Observations::T2
end

struct TDVariable
    ExpectedObservations::Array{Float32}
    Funding::Array{Float32}
    Pay::Array{Float32}
end


function findNan(x::TDVariable)
    t=0
    if any(isnan.(x.ExpectedObservations))
        t=t+1
        println("ExpectedObservations")
    end
    if any(isnan.(x.Funding))
        t=t+1
        println("Funding")
    end
    if any(isnan.(x.Pay))
        t=t+1
        println("TradeState")
    end
    if t==0
        println("Clear")
    end
end



function ValuationInputs(t::Array{Float32},Observations::Array{Float32},trade::TradeInputs)
    Maturity=trade.Maturity
    TradeState=trade.TradeState
    TradeParameters=trade.TradeParameters
    return ValuationInputs(t,Maturity,Observations,TradeState,TradeParameters)
end

function ValuationInputs(t::Array{Float32},Observations::Dual,trade::TradeInputs)
    Maturity=Dual(trade.Maturity,Observations.id)
    TradeState=Dual(trade.TradeState,Observations.id)
    TradeParameters=Dual(trade.TradeParameters,Observations.id)
    return ValuationInputs(Dual(t,Observations.id),Maturity,Observations,TradeState,TradeParameters)
end

function ValuationInputs(market::MarketInputs,trade::TradeInputs)
    t=market.t
    Observations=market.Observations
    Maturity=trade.Maturity
    TradeState=trade.TradeState
    TradeParameters=trade.TradeParameters
    return ValuationInputs(t,Maturity,Observations,TradeState,TradeParameters)
end

function Split(x::ValuationInputs)
    return MarketInputs(x.t,x.Observations), TradeInputs(x.Maturity,x.TradeState,x.TradeParameters)
end

function Mean(m::ValuationInputs)
    Observations=dropdims(Statistics.mean(m.Observations,dims=3),dims=3)
    TradeState=dropdims(Statistics.mean(m.TradeState,dims=3),dims=3)
    return ValuationInputs(m.t,m.Maturity,Observations,TradeState,m.TradeParameters)
end


function printinput(V1::ValuationInputs,V2::ValuationInputs)
    println("t")
    println(V1.t)
    println(V2.t)
    println("Maturity")
    println(V1.Maturity)
    println(V2.Maturity)
    println("Observations")
    println(V1.Observations)
    println(V2.Observations)
    println("TradeState")
    println(V1.TradeState)
    println(V2.TradeState)
    println("TradeParemeters")
    println(V1.TradeParameters)
    println(V2.TradeParameters)
end

function findNan(x::ValuationInputs)
    t=0
    if any(isnan.(x.Maturity))
        t=t+1
        println("Maturity")
    end
    if any(isnan.(x.Observations))
        t=t+1
        println("Observations")
    end
    if any(isnan.(x.TradeState))
        t=t+1
        println("TradeState")
    end
    if any(isnan.(x.TradeParameters))
        t=t+1
        println("TradeParameters")
    end
    if t==0
        println("Clear")
    end

end

function printsize(x::ValuationInputs)
    println("ValuationInputSize")
    println(size(x.Maturity))
    println(size(x.Observations))
    println(size(x.TradeState))
    println(size(x.TradeParameters))
end

function *(A::Matrix{Float32},B::Array{Float32,3})
s1,s2,s3=size(B)
D=reshape(B,s1,s2*s3)
E=A*D
u1,u2=size(E)
C=reshape(E,u1,s2,s3)t(Vauatio)
    return C
end

function *(A::Matrix,B::Array{Float32,3})
    s1,s2,s3=size(B)
    D=reshape(B,s1,s2*s3)
    E=A*D
    u1,u2=size(E)
    C=reshape(E,u1,s2,s3)
        return C
end

function +(A::ValuationInputs,B::ValuationInputs)
    return ValuationInputs(A.t+B.t,A.Maturity+B.Maturity,A.Observations+B.Observations,A.TradeState+B.TradeState,A.TradeParameters+B.TradeParameters)
end

function -(A::ValuationInputs,B::ValuationInputs)
    return ValuationInputs(A.t-B.t,A.Maturity-B.Maturity,A.Observations-B.Observations,A.TradeState-B.TradeState,A.TradeParameters-B.TradeParameters)
end

function ^(A::ValuationInputs,k)
    if isa(A.t,Dual)
    return ValuationInputs(A.t^k,A.Maturity^k,A.Observations^k,A.TradeState^k,A.TradeParameters^k)
    else
    return ValuationInputs(A.t.^k,A.Maturity.^k,A.Observations.^k,A.TradeState.^k,A.TradeParameters.^k)  
    end
end

function /(A::ValuationInputs,N)
    return ValuationInputs(A.t./N,A.Maturity./N,A.Observations./N,A.TradeState./N,A.TradeParameters./N)
end

function /(A::ValuationInputs,B::ValuationInputs)
    return ValuationInputs(EWD(A.t,B.t),EWD(A.Maturity,B.Maturity),EWD(A.Observations,B.Observations),EWD(A.TradeState,B.TradeState),EWD(A.TradeParameters,B.TradeParameters))
end

function *(A::ValuationInputs,B::ValuationInputs)
    return ValuationInputs(A.t.*B.t,A.Maturity.*B.Maturity,A.Observations.*B.Observations,A.TradeState.*B.TradeState,A.TradeParameters.*B.TradeParameters)
end
function *(A::Float32,B::ValuationInputs)
    return ValuationInputs(A.*B.t,A.*B.Maturity,A.*B.Observations,A.*B.TradeState,A.*B.TradeParameters)
end

function sign(A::ValuationInputs)
    return ValuationInputs(sign.(A.t),sign.(A.Maturity),sign.(A.Observations),sign.(A.TradeState),sign.(A.TradeParameters))
end

function sqrt(A::ValuationInputs)
    return ValuationInputs(sqrt.(A.t),sqrt.(A.Maturity),sqrt.(A.Observations),sqrt.(A.TradeState),sqrt.(A.TradeParameters))
end

function mean(A::ValuationInputs)
    return ValuationInputs(Statistics.mean(A.t,dims=2),Statistics.mean(A.Maturity,dims=2),Statistics.mean(A.Observations,dims=2),Statistics.mean(A.TradeState,dims=2),Statistics.mean(A.TradeParameters,dims=2))
end
end
