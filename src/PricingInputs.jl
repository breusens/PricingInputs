module PricingInputs
export Value,saveV, loadV, InputPrint,printsize, InputHeaders, Decimate,Matrices,DataPointers, printsize,printinput,findNan,ValuationInputs,MarketInputs,TradeInputs,Split,Mean,TDVariable,MatchSize,Serialise
# Write your package code here.
import Base: *,+,-,^,/,sqrt,sign,abs,max,min,getindex, Float32, reshape
import Statistics: mean
using Statistics
using MatrixFunctions
using Batcher
using UpdateMarketandTrade
using FloatingNumberType
using MarketData
using Instruments
using JLD2
using FileIO
using PrettyTables
using FloatTracker


struct ValuationInputs{T1,T2,T3,T4,T5,T6}
    t::T1
    Maturity::T2
    Observations::T3
    TradeState::T4
    TradeParameters::T5
    Accrual::T6
end

function printsize(x::ValuationInputs)
    println("t:",size(x.t))
    println("Maturity:",size(x.Maturity))
    println("Observations:",size(x.Observations))
    println("TradeState:",size(x.TradeState))
    println("TradeParameters:",size(x.TradeParameters))
    println("Accrual:",size(x.Accrual))
end

function reshape(x::ValuationInputs,s::Tuple)
    FO=size(x.Observations)[1]
    FS=size(x.TradeState)[1]
    O=reshape(x.Observations,FO,s...)
    TS=reshape(x.TradeState,FS,s...)
    return ValuationInputs(x.t,x.Maturity,O,TS,x.TradeParameters,x.Accrual)
end

function abs(x::ValuationInputs)
    t=abs.(x.t)
    Maturity=abs.(x.Maturity)
    Observations=abs.(x.Observations)
    TradeState=abs.(x.TradeState)
    TradeParameters=abs.(x.TradeParameters)
    Accrual=abs.(x.Accrual)
    return ValuationInputs(t,Maturity,Observations,TradeState,TradeParameters,Accrual)
end

function Value(x::ValuationInputs)
    println(any(isnan.(Value.(x.t))))
    println(any(isnan.(Value.(x.Maturity))))
    println(any(isnan.(Value.(x.Observations))))
    println(any(isnan.(Value.(x.TradeParameters))))
end

function Value(x::TrackedFloat32)
    return x.val
end

struct InputHeaders
    Observations::Vector{String}
    TradeState::Vector{String}
    TradeParameters::Vector{String}
    Accrual::Vector{String}
end

function TP(data::Array,Headers::Vector)
d=hcat(Headers,data)
Hders=(["Field","Value"])
pretty_table(d;backend  = Val(:latex),header=Hders)
return Nothing
end

function InputPrint(x::ValuationInputs,y::InputHeaders)
    println("t1:",x.t)
    println("Maturity:",x.Maturity)
    println("Observations")
    TP(x.Observations,y.Observations)
    println("TradeState")
    TP(x.TradeState,y.TradeState)
    println("TradeParameters")
    TP(x.TradeParameters,y.TradeParameters)
    println("Accrual")
    TP(x.Accrual,y.Accrual)
end

function saveV(V::ValuationInputs,filename::String)
    save(filename,"V",V)
end

function loadV(filename::String)
    return load(filename,"V")
end


function ValuationInputs(t::Batched,x::DataStore)
    Maturity=GetMaturity(t,x.TradeState,x.TradeParams)
    Mc=Join(Maturity,FloatType)
    i=Mc(Maturity)
    Oc=Join(x.Observations,FloatType)
    i=Oc(x.Observations)
    TSc=Join(x.TradeState,FloatType)
    i=TSc(x.TradeState)
    TPc=Join(x.TradeParams,FloatType)
    i=TPc(x.TradeParams)
    Pc=Join(x.Periods,FloatType)
    i=Pc(x.Periods)
    l=length(t.D)
    return ValuationInputs(FloatType.(reshape(t.D,(1,l))),Mc.A,Oc.A,TSc.A,TPc.A,Pc.A)
end

function InputHeaders(x::DataStore)
    Observations=Headers(x.Observations)
    TradeState=Headers(x.TradeState)
    TradeParameters=Headers(x.TradeParams)
    Accrual=Headers(x.Periods)
    return InputHeaders(Observations,TradeState,TradeParameters,Accrual)
end

struct TDVariable
    ExpectedObservations::Array{FloatType}
    Funding::Array{FloatType}
    Pay::Array{FloatType}
end

function TDVariable(ExpectedObservations::Observations,Funding::Array{FloatType},Pay::Array{FloatType})
    EO=Collector(ExpectedObservations,FloatType)
    i=EO(ExpectedObservations)
    return TDVariable(EO.A,FloatType.(Funding),FloatType.(Pay))
end


function Decimate(x::ValuationInputs)
    s=size(x.Observations)    
    if length(s)>2
    (sa,p,b)=size(x.Observations)
    i2=rand(1:b,2)
    else
    (sa,p)=size(x.Observations)
    end
    i1=rand(1:p,5)
    t=x.t[:,i1]
    Maturity=x.Maturity[:,i1]
    TradeParameters=x.TradeParameters[:,i1]
    if length(s)>2
        Observations=x.Observations[:,i1,i2]
        TradeState=x.TradeState[:,i1,i2]
    else
        Observations=x.Observations[:,i1]
        TradeState=x.TradeState[:,i1]
    end
    return ValuationInputs(t,Maturity,Observations,TradeState,TradeParameters)
end

function Decimate(input0::ValuationInputs,input1::ValuationInputs, Extra::TDVariable,Maturity0::Array{FloatType},Maturity1::Array{FloatType},HedgePay)
    (s,p,b)=size(input1.Observations)
    i1=rand(1:p,5)
    i2=rand(1:b,2)
    t=input1.t[:,i1]
    Maturity=input1.Maturity[:,i1]
    TradeParameters=input1.TradeParameters[:,i1]
    Observations=input1.Observations[:,i1,i2]
    TradeState=input1.TradeState[:,i1,i2]
    I1=ValuationInputs(t,Maturity,Observations,TradeState,TradeParameters)
    t=input0.t[:,i1]
    Maturity=input0.Maturity[:,i1]
    TradeParameters=input0.TradeParameters[:,i1]
    Observations=input0.Observations[:,i1]
    TradeState=input0.TradeState[:,i1]
    I0=ValuationInputs(t,Maturity,Observations,TradeState,TradeParameters)
    ExpectedObservations=Extra.ExpectedObservations[:,i1]
    Funding=Extra.Funding[:,i1]
    Pay=Extra.Pay[:,i1,i2]
    M0=Maturity0[:,i1]
    M1=Maturity1[:,i1]
    HP=HedgePay[:,i1,i2]
    return I0,I1,TDVariable(ExpectedObservations,Funding,Pay),M0,M1,HP
end


function Serialise(x::ValuationInputs)
    s1,s2,s3=size(x.Observations)
    t=repeat(x.t,1,s3)
    Maturity=repeat(x.Maturity,1,s3)
    Observations=reshape(x.Observations,s1,s2*s3)
    s1,s2,s3=size(x.TradeState)
    TradeState=reshape(x.TradeState,s1,s2*s3)
    TradeParameters=repeat(x.TradeParameters,1,s3)
    return ValuationInputs(t,Maturity,Observations,TradeState,TradeParameters)
end

function Float32sr(x::ValuationInputs)
    return ValuationInputs(Float32sr.(x.t),Float32sr.(x.Maturity),Float32sr.(x.Observations),Float32sr.(x.TradeState),Float32sr.(x.TradeParameters),Float32sr.(x.Accrual))
end

function Float32(x::ValuationInputs)
    return ValuationInputs(Float32.(x.t),Float32.(x.Maturity),Float32.(x.Observations),Float32.(x.TradeState),Float32.(x.TradeParameters),Float32.(x.Accrual))
end

function Float64(x::ValuationInputs)
    return ValuationInputs(Float64.(x.t),Float64.(x.Maturity),Float64.(x.Observations),Float64.(x.TradeState),Float64.(x.TradeParameters),Float64.(x.Accrual))
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

function MatchSize(k,t::TradeInputs)
    TradeState=repeat(t.TradeState,1,1,k)
    return TradeInputs(t.Maturity,TradeState,t.TradeParameters)
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



function ValuationInputs(t::Array{FloatType},Observations::Array{FloatType},trade::TradeInputs)
    Maturity=trade.Maturity
    TradeState=trade.TradeState
    TradeParameters=trade.TradeParameters
    return ValuationInputs(t,Maturity,Observations,TradeState,TradeParameters)
end



function ValuationInputs(t::Array{FloatType},Observations,trade::TradeInputs)
    Maturity=trade.Maturity
    TradeState=trade.TradeState
    TradeParameters=trade.TradeParameters
    return ValuationInputs(t,Maturity,Observations,TradeState,TradeParameters)
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



function +(A::ValuationInputs,B::ValuationInputs)
    return ValuationInputs(A.t+B.t,A.Maturity+B.Maturity,A.Observations+B.Observations,A.TradeState+B.TradeState,A.TradeParameters+B.TradeParameters,A.Accrual+B.Accrual)
end

function -(A::ValuationInputs,B::ValuationInputs)
    return ValuationInputs(A.t.-B.t,A.Maturity.-B.Maturity,A.Observations.-B.Observations,A.TradeState.-B.TradeState,A.TradeParameters.-B.TradeParameters,A.Accrual-B.Accrual)
end

function ^(A::ValuationInputs,k)
    ValuationInputs(A.t.^k,A.Maturity.^k,A.Observations.^k,A.TradeState.^k,A.TradeParameters.^k,A.Accrual.^k)  
end

function /(A::ValuationInputs,N)
    return ValuationInputs(A.t./N,A.Maturity./N,A.Observations./N,A.TradeState./N,A.TradeParameters./N,A.Accrual/N)
end

function /(A::ValuationInputs,B::ValuationInputs)
    return ValuationInputs(A.t./B.t,A.Maturity./B.Maturity,A.Observations./B.Observations,A.TradeState./B.TradeState,A.TradeParameters./B.TradeParameters,A.Accrual./B.Accrual)
end

function *(A::ValuationInputs,B::ValuationInputs)
    return ValuationInputs(A.t.*B.t,A.Maturity.*B.Maturity,A.Observations.*B.Observations,A.TradeState.*B.TradeState,A.TradeParameters.*B.TradeParameters,A.Accrual.*B.Accrual)
end
function *(A::FloatType,B::ValuationInputs)
    return ValuationInputs(A.*B.t,A.*B.Maturity,A.*B.Observations,A.*B.TradeState,A.*B.TradeParameters,A.*B.Accrual)
end

function sign(A::ValuationInputs)
    return ValuationInputs(sign.(A.t),sign.(A.Maturity),sign.(A.Observations),sign.(A.TradeState),sign.(A.TradeParameters),sign.(A.Accrual))
end

function sqrt(A::ValuationInputs)
    return ValuationInputs(sqrt.(A.t),sqrt.(A.Maturity),sqrt.(A.Observations),sqrt.(A.TradeState),sqrt.(A.TradeParameters),sqrt.(A.Accrual))
end

function lmean(x::Array)
    return Statistics.mean(x,dims=ndims(x))
end

function mean(A::ValuationInputs)
    return ValuationInputs(Statistics.mean(A.t),Statistics.mean(A.Maturity),lmean(A.Observations),lmean(A.TradeState),lmean(A.TradeParameters),lmean(A.Accrual))
end
end
