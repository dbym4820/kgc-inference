:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).

:- rdf_load('./SpeckledBand.rdf').

%===================================================%
%            ここからRDF->Fact（SceneInfo）の定義
%===================================================%
%% KnowledgeGraph独自のオントロジーのURIに変換
conc_kgc_ontology(Concept, Result) :- 
    concat_atom(['http://kgc.knowledge-graph.jp/ontology/kgc.owl', Concept], '#', Result).

conc_rdf_ontology(Concept, Result) :-
    concat_atom(['http://www.w3.org/1999/02/22-rdf-syntax-ns', Concept], '#', Result).

%% 原文データを取得・これだけ構造が違ったので別定義
findSceneSource(SceneNum, Source) :-
    conc_kgc_ontology('source', SourceAtom),
    rdf(SceneNum, SourceAtom, literal(lang(ja, Source))), !.

%% 特定の関係リンクに対応するデータを取得
findSceneConcept(SceneNum, Pred, Concept) :-
    conc_kgc_ontology(Pred, PredicateAtom),
    rdf(SceneNum, PredicateAtom, Concept), !.
findSceneConcept(_, _, 'null') :- !.

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
%        ここから特定シーンに出てくる情報の解析機
%===================================================%
%% 特定シーンに人が出てくるか
findRelatedConcept(TargetConcept, Pred, Concept) :-
    conc_rdf_ontology(Pred, PredicateAtom),
    rdf(TargetConcept, PredicateAtom, Concept), !.
findRelatedConcept(_, _, 'null') :- !.

getSceneRelatedTypes(SceneNum,
		     [TypeofSubject, Subject],
		     [TypeofPredicate, Predicate],
		     [TypeofWhat, What],
		     [TypeofWhere, Where],
		     [TypeofTo, To]) :-
    getInfoFromSceneNum(SceneNum, scene(_, _, Subject, Predicate, What, Where, To)),
    findRelatedConcept(Subject, 'type', TypeofSubject),
    findRelatedConcept(Predicate, 'type', TypeofPredicate),
    findRelatedConcept(What, 'type', TypeofWhat),
    findRelatedConcept(Where, 'type', TypeofWhere),
    findRelatedConcept(To, 'type', TypeofTo).

findConcept(SceneNum, Concept, SubConcepts) :-
    conc_kgc_ontology(Concept, ConceptAtom),
    bagof(Subject,
	  getSceneRelatedTypes(SceneNum,
			       [ConceptAtom | [Subject]],
			       _, _, _, _),
	  [SubConcepts]).

%% Subjectのときのみだけど，そのシーンに人がいるかどうか，
%% いるとしたら誰がいるのか，を捉えられるように成ったやつ
isAppearPerson(SceneNum, Person) :-
    findConcept(SceneNum, 'Person', Person).

%===================================================%
%        ここからオントロジー -> Rule/Factの定義（これは，すなわち仮説とほぼ同じ？）
%===================================================%
%% load_trick_ontology(OntologyFilePath).
%% このTrickのルールが，オントロジーファイルをLoadして，動的に作り上げることになる述語群
%% 
trick('犯人が被害者に化けるトリック') :-
    dead(X), a.

trick('〜トリック') :-
    aaaaa.


%===================================================%
%        ここから，Feedback生成器
%===================================================%
%% 学習者が，特定シーンまで見たとき，検討することが望ましい可能世界
available_world(_読み進めているシーン番号, [Person, Trick]) :-
    getAllSceneAboveTime(_読み進めているシーン番号, Scenes),
    %%  ここで，Scenesから得た情報をもとに，トリックに関係するFactをAssertする？とにかく，次のTrickが成立・非成立するように事実を追加する処理を行う
    trick(Trick).

%% %% hypothesis(トリック行為者, トリック). %% 誰が，何をした可能性があるのか・ロイロットが１人二役（によって県議を逃れようとしている可能性）
%% %% ↓こういう形態になってたら，仮説として提示できんじゃね？ってこと
%% %% hypothesis(persoin(roylotte), trick('犯人が被害者に化けるトリック')).
hypothesis_inference(_読み進めているシーン番号, Hypothesis) :-
    bagof(hypothesis(Person, Trick),
	  available_world(_読み進めているシーン番号, [Person, Trick]),
	 Hypothesis).
	

%%%%%%%%% 説明性を以下に高めてフィードバックできるか（推論プロセスの情報を以下に取得するか）が課題


%%%＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝%%%
%%%    残った課題は，３つ
%%%    ・トリックの中身を（１事例でもいいので）定義する
%%%    ・特定のシーンまで読解したときの，そこまでの情報をFactとして，PrologにAssertする仕組み（維持例を出すための，ほんの一部で良いと思う）
%%%    ・説明性を保ったフィードバック（仮説）の提示処理
%%%＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝＝%%%
