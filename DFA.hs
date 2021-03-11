import Data.List

type State = String
type Symbol = Char
type Alpha = String
type Input = Symbol
type InputString = [Symbol]

type Transition = (State, Symbol, State)
type DFA = ([State]     -- all states represented as a list of strings
        , Alpha         --alpha representing the alphabet as one single string
        , State         -- initial state representing as a string
        , [State]       -- final states representing accepting states as list of strings
        , [Transition]  -- all transititions represented as a list of three-tuples of string
        )

dfaStateFactory :: DFA
dfaStateFactory = (["q0", "q1", "q2", "q3", "q4"],
   ['0','1'],
   "q0",
   ["q3"],
    [("q0", '0', "q1"),
    ("q0", '1', "q1"),
    ("q0", '.', "q2"),
    ("q1", '0', "q1"),
    ("q1", '1', "q1"),
    ("q1", '.', "q3"),
    ("q2", '0', "q3"),
    ("q2", '1', "q3"),
    ("q2", '.', "q4"),
    ("q3", '0', "q3"),
    ("q3", '1', "q3"),
    ("q3", '.', "q4"),
    ("q4", '0', "q4"),
    ("q4", '1', "q4"),
    ("q4", '.', "q4")] )


alphabet :: DFA -> Alpha
alphabet (listOfAllStates, alpha, initialState, finalStates, transitionList) = alpha

allStates :: DFA -> [State]
allStates (listOfAllStates, alpha, initialState, finalStates, transitionList) = listOfAllStates

firstState :: DFA -> State
firstState (listOfAllStates, alpha, initialState, finalStates, transitionList) = initialState

acceptStates :: DFA -> [State]
acceptStates (listOfAllStates, alpha, initialState, finalStates, transitionList) = finalStates

allTransitions :: DFA -> [Transition]
allTransitions (listOfAllStates, alpha, initialState, finalStates, transitionList) = transitionList

transFromState :: Transition -> State
transFromState (fromState, label, toState) = fromState

transLabel :: Transition -> Symbol
transLabel (fromState, label, toState) = label

transToState :: Transition -> State
transToState (fromState, label, toState) = toState

findTransition :: State -> Symbol -> [Transition] -> [Transition]   
findTransition currentState label transitionList = case find testTrans transitionList of 
    Nothing -> []
    Just x -> [x]

    where
        testTrans :: Transition -> Bool
        testTrans transition = transFromState transition == currentState && transLabel transition == label


findNextState :: DFA -> Input -> [State]                            
findNextState currentAttributes currentInput = case findTransition (firstState currentAttributes) currentInput (allTransitions currentAttributes) of
    [] -> []
    [x] -> [transToState x] 

dfaAccept :: DFA -> InputString -> Bool                          
dfaAccept currentAttributes []  =  elem (firstState currentAttributes) (acceptStates currentAttributes)
dfaAccept currentAttributes@ (listOfAllStates, alpha, initialState, finalStates, transitionList) (element:list) = case findNextState currentAttributes element of
    [] -> False
    [x] -> dfaAccept (listOfAllStates, alpha, x, finalStates, transitionList) list




