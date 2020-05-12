:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- rdf_load('./SpeckledBand.rdf').

%===================================================%
%            ここからRDF->Fact（SceneInfo）の定義
%===================================================%
%% KnowledgeGraph独自のオントロジーのURIに変換
conc_kgc_ontology(Concept, Result) :- 
    concat_atom(['http://kgc.knowledge-graph.jp/ontology/kgc.owl', Concept], '#', Result).

%% 原文データを取得・これだけ構造が違ったので別定義
findSceneSource(SceneNum, Source) :-
    conc_kgc_ontology('source', SourceAtom),
    rdf(SceneNum, SourceAtom, literal(lang(ja, Source))), !.

%% 特定の関係リンクに対応するデータを取得
findSceneConcept(SceneNum, Pred, Concept) :-
    conc_kgc_ontology(Pred, PredicateAtom),
    rdf(SceneNum, PredicateAtom, Concept), !.
findSceneConcept(_, _, "NULL") :- !.

%% 特定シーンに対して，付与されている概念を見つけ出して一元化
findSceneInfo(SceneNum, Source, Subject, Predicate, What, Where, To) :-
    conc_kgc_ontology('Situation', SituationAtom), %% シーンナンバーのクラスを取得
    rdf(SceneNum, _, SituationAtom), %% シーンナンバーのアトムを取得
    findSceneSource(SceneNum, Source),
    findSceneConcept(SceneNum, 'subject', Subject),
    findSceneConcept(SceneNum, 'hasPredicate', Predicate),
    findSceneConcept(SceneNum, 'what', What),
    findSceneConcept(SceneNum, 'where', Where),
    findSceneConcept(SceneNum, 'to', To).

getSceneInfo(Scenes, SceneNum, Source, Subject, Predicate, What, Where, To) :-
    bagof(scene(SceneNum, Source, Subject, Predicate, What, Where, To),
	  findSceneInfo(SceneNum, Source, Subject, Predicate, What, Where, To),
	  Scenes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%% シーン情報抽出のメイン述語
%%%%%%%%%%% Usage: getInfoFromSceneNum(48, Scene).
%%%%%%%%%%% Return: Scene = scene(シーン番号, 原文, Subject, Predicate, What, Where, To).
getInfoFromSceneNum(SceneNum, SceneInfo) :-
    concat_atom(['http://kgc.knowledge-graph.jp/data/SpeckledBand', SceneNum], '/', Result),
    getSceneInfo([SceneInfo], Result, _, _, _, _, _, _).

%% 例：特定シーンのPredicateを取得する述語
getScenePredicate(SceneNum, Predicate) :-
    getInfoFromSceneNum(SceneNum, scene(_, _, _, Predicate, _, _, _)).


%% 特定時刻までのすべてのシーンの情報を配列に読み込み
getAllSceneAboveTime(_読み進めているシーン番号, Scenes) :-
    findall(Scene,
	    (numlist(0, _読み進めているシーン番号, NumLis), member(X, NumLis),
	     getInfoFromSceneNum(X, Scene)),
	    Scenes).

%===================================================%
%        ここからオントロジー -> Rule/Factの定義（これは，すなわち仮説とほぼ同じ？）
%===================================================%
%% load_trick_ontology(OntologyFilePath).
%% trick(トリック名, )
%% trick('一人二役トリック', )


%% trick('犯人が被害者に化けるトリック') :-
%%     a.

%% trick('〜トリック') :-
%%     aaaaa.


%% available_world(_読み進めているシーン番号, Person, Trick) :-
%%     a.

%% available_reason(available_world(_読み進めているシーン番号, Person, Trick), Reason) :-
%%     a.

%% %% hypothesis(トリック行為者, トリック). %% 誰が，何をした可能性があるのか・ロイロットが１人二役（によって県議を逃れようとしている可能性）
%% %% こういう形態になってたら，仮説として提示できんじゃね？ってこと
%% %% hypothesis(persoin(roylotte), trick('犯人が被害者に化けるトリック')).
%% hypothesis_inference(_読み進めているシーン番号, Hypothesis) :-
%%     bagof(hypothesis(Person, Trick),
%% 	  available_world(_読み進めているシーン番号, Person, Trick),
%% 	 Hypothesis).
	
