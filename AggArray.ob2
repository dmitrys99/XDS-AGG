MODULE AggArray;

IMPORT
  bas := AggBasics;

TYPE
  array_item_ptr* = bas.array_item_ptr;
  array_item* = bas.array_item;

  array_type_ptr* = POINTER TO array_type;
  array_type* = ARRAY OF array_item_ptr;

  array_base_ptr* = POINTER TO array_base;
  array_base* = RECORD
    (* size - количество элементов, находящихся массиве. *)
    size- : bas.int32;
    array-: array_type_ptr;
  END;

  array_adaptor_ptr* = POINTER TO array_adaptor;
  array_adaptor* = RECORD(array_base)
  END;

  pod_auto_array_ptr* = POINTER TO pod_auto_array;
  pod_auto_array* = RECORD(array_base)
  END;

  pod_array_ptr* = POINTER TO pod_array;
  pod_array* = RECORD(array_base)
    (*size-,*)
    capacity-: bas.int32;
    (*array-: array_type_ptr;*)
  END;

  pod_deque_ptr* = POINTER TO pod_deque;
  pod_deque* = RECORD(pod_array)
    increment: bas.int32; (*Количество элементов, добавляемом к деку при очередном выделении памяти*)
    count-: bas.int32; (* Размер дека (выделено места под элементы) *)
  END;


PROCEDURE (ab: array_base_ptr) Destruct*();
BEGIN
  ab.size  := 0;
  ab.array := NIL;
END Destruct;

(*===============*)
(* array_adaptor *)
(*===============*)

PROCEDURE (aa: array_adaptor_ptr) Construct*(array: array_type_ptr; size: bas.int32);
BEGIN
  ASSERT(array # NIL);
  ASSERT(size >= 0);
  aa.array := array;
  aa.size := size;
END Construct;

(*================*)
(* pod_auto_array *)
(*================*)

PROCEDURE (paa: pod_auto_array_ptr) Construct*(size: bas.int32);
BEGIN
  paa.size := size;
  NEW(paa.array, paa.size);
END Construct;

(*===========*)
(* pod_array *)
(*===========*)

PROCEDURE (pa: pod_array_ptr) Construct*();
BEGIN
  pa.size     := 0;
  pa.capacity := 0;
  pa.array    := NIL;
END Construct;

PROCEDURE (pa: pod_array_ptr) Capacity*(cap, extra_tail: bas.int32);
BEGIN
  pa.size := 0;

  IF cap > pa.capacity THEN
    pa.capacity := cap + extra_tail;

    IF pa.capacity > 0 THEN
      NEW(pa.array, pa.capacity)
    ELSE
      pa.array := NIL;
    END;
  END;
END Capacity;

PROCEDURE (pa: pod_array_ptr) allocate*(size, extra_tail: bas.int32);
BEGIN
  pa.Capacity(size, extra_tail);
  pa.size := size;
END allocate;

PROCEDURE (VAR pa: pod_array) zero*();
VAR
  i: bas.int32;
BEGIN
  i := 0;
  WHILE i < pa.size DO
    pa.array[i] := NIL;
    INC(i);
  END;
END zero;

PROCEDURE (pd: pod_deque_ptr) ConstructShift*(shift: bas.int32);
BEGIN
  pd.count     := 0;
  pd.size      := 0;
  pd.increment := ASH(1, shift);
  pd.array     := NIL;
END ConstructShift;

PROCEDURE (pd: pod_deque_ptr) Construct*();
BEGIN
  pd.ConstructShift(6);
END Construct;

PROCEDURE (pd: pod_deque_ptr) allocate_cnt*(count: bas.int32);
VAR
  new_array: array_type_ptr;
  i: bas.int32;
BEGIN
  IF count >= pd.count THEN
    NEW(new_array, count);
    i := 0;
    WHILE i < pd.size DO
      new_array[i] := pd.array[i];
      INC(i);
    END;
    pd.count := count;
    pd.array := new_array;
  END;
END allocate_cnt;

PROCEDURE (pd: pod_deque_ptr) add*(item: array_item_ptr);
BEGIN
  IF pd.size = pd.count THEN
    pd.allocate_cnt(pd.size + pd.increment);
  END;

  INC(pd.size);

  pd.array[pd.size - 1] := item;
END add;

PROCEDURE (pd: pod_deque_ptr) remove_last*();
BEGIN
  IF pd.size > 0 THEN
    DEC(pd.size);
  END;
END remove_last;

PROCEDURE (pd: pod_deque_ptr) remove_all*();
BEGIN
  pd.size := 0;
END remove_all;

PROCEDURE (pd: pod_deque_ptr) modify_last*(item: array_item_ptr);
BEGIN
  pd.remove_last();
  pd.add(item);
END modify_last;

PROCEDURE (pd: pod_deque_ptr) curr*(i: bas.int32): array_item_ptr;
BEGIN
  RETURN pd.array[i MOD pd.size];
END curr;

PROCEDURE (pd: pod_deque_ptr) next*(i: bas.int32): array_item_ptr;
BEGIN
  (* Поскольку работаем с очередью, то обращение к элементам защищается
     получением остатка от размера очереди. Это "заворачивает" индекс на ноль.
     А для обращения к элементам "за" нулем, добавляем к требуемому индексу
     размер очереди. *)
  RETURN pd.array[(i + 1) MOD pd.size];
END next;

PROCEDURE (pd: pod_deque_ptr) prev*(i: bas.int32): array_item_ptr;
BEGIN
  RETURN pd.array[(i + pd.size - 1) MOD pd.size];
END prev;



END AggArray.
