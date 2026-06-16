open System

// Определение структуры обобщенного бинарного дерева
type Tree<'T> =
    | Empty
    | Node of value: 'T * left: Tree<'T> * right: Tree<'T>

module Tree =
    // Задание 1: Функция map для дерева 
    // Рекурсивно применяет функцию f к каждому узлу, создавая новое дерево.
    let rec map (f: 'T -> 'U) (tree: Tree<'T>) : Tree<'U> =
        match tree with
        | Empty -> Empty
        | Node(v, left, right) -> 
            Node(f v, map f left, map f right)

    // Задание 2: Функция fold для дерева 
    // Выполняет свертку дерева, проходя по всем узлам и накапливая состояние (state).
    let rec fold (folder: 'State -> 'T -> 'State) (state: 'State) (tree: Tree<'T>) : 'State =
        match tree with
        | Empty -> state
        | Node(v, left, right) ->
            let stateAfterLeft = fold folder state left
            let stateAfterRight = fold folder stateAfterLeft right
            folder stateAfterRight v

module Program =
    // --- Вариант 10: Логика для Задания 1 ---
    // К каждому числу приписать заданную цифру в конец [cite: 4]
    let appendDigit (digit: int) (number: int) : int =
        let d = abs (digit % 10) // Гарантируем, что берем только одну положительную цифру
        if number >= 0 then 
            number * 10 + d
        else 
            number * 10 - d

    // --- Вариант 10: Логика для Задания 2 ---
    // Проверка, содержит ли число заданную цифру
    let containsDigit (digit: int) (number: int) : bool =
        let target = abs (digit % 10)
        
        let rec check (n: int) =
            if n = 0 then false
            elif abs (n % 10) = target then true
            else check (n / 10)

        if number = 0 then target = 0
        else check number

    // Подсчет элементов, которые НЕ содержат заданную цифру [cite: 7]
    let countWithoutDigit (digit: int) (tree: Tree<int>) : int =
        let folder state value =
            if not (containsDigit digit value) then state + 1
            else state
        Tree.fold folder 0 tree

    // --- Генерация случайного дерева ---
    let rng = Random()

    let rec generateRandomTree (depth: int) : Tree<int> =
        if depth <= 0 then 
            Empty
        else
            // Случайные значения от -50 до 50 для наглядности
            let value = rng.Next(-50, 51) 
            // Вероятность 20% создать пустой узел для генерации неидеально сбалансированного дерева
            if rng.NextDouble() < 0.2 then Empty
            else Node(value, generateRandomTree (depth - 1), generateRandomTree (depth - 1))

    // Вспомогательная функция для вывода дерева (in-order)
    let rec printTree (tree: Tree<int>) =
        match tree with
        | Empty -> ()
        | Node(v, left, right) ->
            printTree left
            printf "%d " v
            printTree right

// Точка входа для проверки
let randomTree = Program.generateRandomTree 4

printfn "Исходное случайное дерево (in-order обход):"
Program.printTree randomTree
printfn "\n"

let targetDigit = 3

// Тест Задания 1
let mappedTree = Tree.map (Program.appendDigit targetDigit) randomTree
printfn "Задание 1: Дерево после приписывания цифры %d в конец[cite: 4]:" targetDigit
Program.printTree mappedTree
printfn "\n"

// Тест Задания 2
let count = Program.countWithoutDigit targetDigit randomTree
printfn "Задание 2: Количество элементов исходного дерева, НЕ содержащих цифру %d: %d [cite: 7]" targetDigit count