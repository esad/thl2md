#!/usr/bin/env swipl 

:- use_module(library(main)).
:- initialization(main, main).

:- use_module(library(xpath)).
:- use_module(library(solution_sequences)).
:- use_module(library(dcg/basics), [atom//1]).

:- dynamic(list/5).
:- dynamic(folder/3).
:- dynamic(task/5).

:- debug(thl2md).

parse_children([], A, A).
parse_children([element(key,[], [Key]), element(dict,_,Children) | Rest], Acc, Acc1) :- !,
  parse_children(Children, _{}, Result),
  put_dict(Key, Acc, Result, Acc0),
  parse_children(Rest, Acc0, Acc1).

parse_children([element(key, [], [Key]), element(Type, _, [Value]) | Rest], Acc, Acc1) :- !,
  Term =.. [Type, Value],
  put_dict(Key, Acc, Term, Acc0),
  parse_children(Rest, Acc0, Acc1).

parse_children([_|Rest], Acc, Acc1) :-
  parse_children(Rest, Acc, Acc1).

parse_plist(File, Dict) :-
  load_xml(File, Doc, [space(remove)]),
  xpath(Doc, //plist/dict, element(dict, [], Children)), !,
  parse_children(Children, _{}, Dict).

load_thl_backup(File) :-
  transaction((
    abolish(list/5),
    abolish(folder/3),
    abolish(task/5),
    parse_plist(File, X),
    Fs = X.get('PFEntities'/'Folder'),
    forall((
      get_dict(Uid, Fs, F), _{parentGroup: string(ParentUid), title: string(Title)} :< F
      ), assertz(folder(Uid, ParentUid, Title))
    ),
    Ls = X.get('PFEntities'/'List'),
    forall(
      (get_dict(Uid, Ls, L),
      _{title: string(Title), createdDate:date(CreatedAt), modifiedDate:date(UpdatedAt), parentGroup:string(ParentUid)} :< L
      ),
      assertz(list(Uid, ParentUid, Title, CreatedAt, UpdatedAt))
    ),
    Ts = X.get('PFEntities'/'Task'),
    forall(
      (
        get_dict(Uid, Ts, T),
        (_{status: string(S)} :< T -> (S = 'C' -> Status = done ; S = 'X' -> Status = strike ; Status = unknown) ; Status = open),
        _{title: string(Title), displayOrder: real(OrderStr)} :< T,
        ( _{parentTask: string(ParentUid)} :< T -> 
          Parent = task(ParentUid) 
        ; _{parentList: string(ParentListUid)} :< T ->
          Parent = list(ParentListUid)
        ),
        atom_number(OrderStr, Order)
      ),
      assertz(task(Uid, Parent, Title, Status, Order))
    )
  )).

folder_path(Uid, FullPath) :-
  ( folder(Uid, ParentUid, Title) ->
    folder_path(ParentUid, ParentPath),
    atomic_list_concat([ParentPath,'/',Title], FullPath)
  ; FullPath = 'output'
  ).

list_path(Uid, FullPath) :-
  list(Uid, ParentUid, Title, _, _),
  folder_path(ParentUid, FolderPath),
  re_replace("/", "_", Title, SafeTitle),
  atomic_list_concat([FolderPath,'/',SafeTitle,'.md'], FullPath).

tasktree(Parent, Tasks) :-
  findall(
    T,
    order_by([asc(Order)], (
      task(TUid, Parent, Title, Status, Order),
      tasktree(task(TUid), Children),
      T = _{title: Title, status: Status, children: Children, order: Order}
    )),
    Tasks
  ).

make_folders :-
  foreach(
    (folder(Uid, _, _), folder_path(Uid, FullPath)),
    make_directory_path(FullPath)
  ).

list_md(Uid, CreatedAt, UpdatedAt) --> 
  {
    list(Uid, _, Title, CreatedAt, UpdatedAt),
    tasktree(list(Uid), Tasks)
  },
  %% "# ",
  %% atom(Title),
  %% "\n\n",
  tasks_md(0, Tasks).

tasks_md(_Level, []) --> [].
tasks_md(Level, [T | Rest]) --> !,
  {Spaces is Level * 1, Level1 is Level + 1},
  {_{status: Status, title: Title, children: Children} :< T},
  indent(Spaces),
  "* ",
  status_md(Status),
  atom(Title),
  "\n",
  tasks_md(Level1, Children),
  tasks_md(Level, Rest).

status_md(open) --> "", !.
status_md(done) --> "[x] ", !.
status_md(strike) --> "[X] ", !.
status_md(unknown) --> "[?] ", !.
status_md(X) --> {throw(unknown_status(X))}.

indent(0) --> !, "".
indent(N1) -->
  "\t", !,
  {N2 is N1 - 1},
  indent(N2).

main([Backup]) :-
  load_thl_backup(Backup),
  make_folders,
  forall(
    phrase(list_md(Uid, _CreatedAt, UpdatedAt), ListA),
    (
      string_codes(Str, ListA),
      list_path(Uid, FullPath),
      open(FullPath, write, Out), write(Out, Str), close(Out),
      %parse_time(CreatedAt, iso_8601, CreatedAtTs),
      parse_time(UpdatedAt, iso_8601, UpdatedAtTs),
      set_time_file(FullPath, [], [modified(UpdatedAtTs)]),
      debug(thl2md, "Wrote ~w", [FullPath])
    )
  ).
