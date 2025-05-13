%%%-------------------------------------------------------------------
%%% @author Bill Wang
%%% @copyright (C) 2017, Freecnpro
%%% @doc
%%%
%%% @end
%%% Created : 2017-04-17
%%%-------------------------------------------------------------------
%% This file includes a large portion of Erlang/OTP source code file
%% lib/observer/src/observer_alloc_wx.erl
%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2015-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%

-module(observerweb_alloc).
-author("bill@freecnpro.net").

-export([memory_alloc_info/1]).

memory_alloc_info(Node) ->
	SysInfo = observerweb:try_rpc(Node, observer_backend, sys_info, []),
  	AllocFields = alloc_info(SysInfo),
  	AllocFields.

%%====================================================================
%% copied from lib/observer/src/observer_alloc_wx.erl

alloc_info(SysInfo) ->
    AllocInfo = proplists:get_value(alloc_info, SysInfo, []),
    alloc_info(AllocInfo, [], 0, 0, true).

alloc_info([{Type,Instances}|Allocators],TypeAcc,TotalBS,TotalCS,IncludeTotal) ->
    {BS,CS,NewTotalBS,NewTotalCS,NewIncludeTotal} =
        sum_alloc_instances(Instances,0,0,TotalBS,TotalCS),
    alloc_info(Allocators,[{Type,BS,CS}|TypeAcc],NewTotalBS,NewTotalCS,
               IncludeTotal andalso NewIncludeTotal);
alloc_info([],TypeAcc,TotalBS,TotalCS,IncludeTotal) ->
    Types = [X || X={_,BS,CS} <- TypeAcc, (BS>0 orelse CS>0)],
    case IncludeTotal of
        true ->
            [{total,TotalBS,TotalCS} | lists:reverse(Types)];
        false ->
            lists:reverse(Types)
    end.

sum_alloc_instances(false,BS,CS,TotalBS,TotalCS) ->
    {BS,CS,TotalBS,TotalCS,false};
sum_alloc_instances([{_,_,Data}|Instances],BS,CS,TotalBS,TotalCS) ->
    {NewBS,NewCS,NewTotalBS,NewTotalCS} =
        sum_alloc_one_instance(Data,BS,CS,TotalBS,TotalCS),
    sum_alloc_instances(Instances,NewBS,NewCS,NewTotalBS,NewTotalCS);
sum_alloc_instances([],BS,CS,TotalBS,TotalCS) ->
    {BS,CS,TotalBS,TotalCS,true}.

sum_alloc_one_instance([{_,[{blocks,TypedBlocks},{carriers_size,CS,_,_}]}|
                        Rest],OldBS,OldCS,TotalBS,TotalCS) ->
    %% OTP 23 and later.
    BS = sum_alloc_block_list(TypedBlocks, 0),
    sum_alloc_one_instance(Rest,OldBS+BS,OldCS+CS,TotalBS+BS,TotalCS+CS);
sum_alloc_one_instance([{_,[{blocks_size,BS,_,_},{carriers_size,CS,_,_}]}|
                        Rest],OldBS,OldCS,TotalBS,TotalCS) ->
    %% OTP 22 and earlier.
    sum_alloc_one_instance(Rest,OldBS+BS,OldCS+CS,TotalBS+BS,TotalCS+CS);
sum_alloc_one_instance([_|Rest],BS,CS,TotalBS,TotalCS) ->
    sum_alloc_one_instance(Rest,BS,CS,TotalBS,TotalCS);
sum_alloc_one_instance([],BS,CS,TotalBS,TotalCS) ->
    {BS,CS,TotalBS,TotalCS}.

sum_alloc_block_list([{_Type, [{size, Current, _, _}]} | Rest], Acc) ->
    %% We ignore the type since we're returning a summary of all blocks in the
    %% carriers employed by a certain instance.
    sum_alloc_block_list(Rest, Current + Acc);
sum_alloc_block_list([{_Type, [{size, Current}]} | Rest], Acc) ->
    sum_alloc_block_list(Rest, Current + Acc);
sum_alloc_block_list([_ | Rest], Acc) ->
    sum_alloc_block_list(Rest, Acc);
sum_alloc_block_list([], Acc) ->
    Acc.
