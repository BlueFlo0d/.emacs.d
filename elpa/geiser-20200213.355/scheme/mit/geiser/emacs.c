/* Emacs: this is -*- C -*- code, */
/* generated 2020-02-21T09:52:03-05 by Liar version 4.118. */

#include "liarc.h"

#define LABEL_1_4 3
#define LABEL_1_5 5
#define LABEL_1_6 7
#define TAG_1_7 2
#define LABEL_1_12 9
#define LABEL_1_13 11
#define LABEL_1_9 13
#define LABEL_1_10 15
#define ENVIRONMENT_LABEL_1_3 28
#define DEBUGGING_LABEL_1_2 27
#define OBJECT_1_1 26
#define OBJECT_1_0 25
#define EXECUTE_CACHE_1_15 17
#define EXECUTE_CACHE_1_14 19
#define EXECUTE_CACHE_1_11 21
#define EXECUTE_CACHE_1_8 23
#define FREE_REFERENCES_LABEL_1_0 16
#define NUMBER_OF_LINKER_SECTIONS_1_1 1

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_1 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  machine_word Wrd5;
  machine_word Wrd9;
  machine_word Wrd21;
  machine_word Wrd20;
  machine_word Wrd23;
  machine_word Wrd25;
  machine_word Wrd30;
  machine_word Wrd29;
  machine_word Wrd28;
  machine_word Wrd27;
  machine_word Wrd26;
  machine_word Wrd10;
  machine_word Wrd17;
  machine_word Wrd19;
  machine_word Wrd18;
  machine_word Wrd12;
  machine_word Wrd13;
  machine_word Wrd11;
  machine_word Wrd7;
  machine_word Wrd6;
  machine_word Wrd8;
  INVOKE_INTERFACE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_1_4);
      goto all_completions_8;

    case 1:
      current_block = (Rpc - LABEL_1_5);
      goto continuation_6;

    case 2:
      current_block = (Rpc - LABEL_1_6);
      goto lambda_12;

    case 3:
      current_block = (Rpc - LABEL_1_12);
      goto continuation_3;

    case 4:
      current_block = (Rpc - LABEL_1_13);
      goto continuation_2;

    case 5:
      current_block = (Rpc - LABEL_1_9);
      goto continuation_1;

    case 6:
      current_block = (Rpc - LABEL_1_10);
      goto continuation_0;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (all_completions_11)
DEFLABEL (all_completions_8)
  INTERRUPT_CHECK (26, LABEL_1_4);
  (Wrd8.Obj) = (current_block [OBJECT_1_0]);
  (* (Rhp++)) = (Wrd8.Obj);
  (Wrd6.pObj) = (& (Rhp [-1]));
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (54, (Wrd6.pObj)));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd11.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_1_5]))));
  (* (--Rsp)) = (Wrd11.Obj);
  (* (Rhp++)) = (MAKE_OBJECT (TC_MANIFEST_CLOSURE, 6));
  (Wrd13.pObj) = (Rhp + 1);
  Rhp += 1;
  WRITE_LABEL_DESCRIPTOR (Rhp, 0x202, 2);
  (* (Rhp++)) = (dispatch_base + TAG_1_7);
  (* (Rhp++)) = ((SCHEME_OBJECT) (& (current_block [LABEL_1_6])));
  Rhp += 3;
  (Wrd12.Obj) = (MAKE_POINTER_OBJECT (40, (Wrd13.pObj)));
  Wrd18 = Wrd13;
  (Wrd19.Obj) = (Rsp [3]);
  ((Wrd18.pObj) [2]) = (Wrd19.Obj);
  (Wrd17.Obj) = (Rsp [2]);
  ((Wrd18.pObj) [3]) = (Wrd17.Obj);
  ((Wrd18.pObj) [4]) = (Wrd7.Obj);
  (* (--Rsp)) = (Wrd12.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_1_8]));

DEFLABEL (continuation_6)
  INTERRUPT_CHECK (27, LABEL_1_5);
  (Wrd6.Obj) = (Rsp [0]);
  (Wrd7.pObj) = (OBJECT_ADDRESS (Wrd6.Obj));
  Rvl = ((Wrd7.pObj) [0]);
  Rsp = (& (Rsp [3]));
  goto pop_return;

DEFLABEL (lambda_12)
  CLOSURE_HEADER (LABEL_1_6);

DEFLABEL (lambda_5)
  CLOSURE_INTERRUPT_CHECK (24);
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_1_9]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd10.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_1_10]))));
  (* (--Rsp)) = (Wrd10.Obj);
  (Wrd11.Obj) = (Rsp [3]);
  (* (--Rsp)) = (Wrd11.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_1_11]));

DEFLABEL (continuation_0)
  INTERRUPT_CHECK (27, LABEL_1_10);
  (* (--Rsp)) = Rvl;
  (Wrd5.Obj) = (Rsp [2]);
  (Wrd6.pObj) = (OBJECT_ADDRESS (Wrd5.Obj));
  (Wrd7.Obj) = ((Wrd6.pObj) [3]);
  (* (--Rsp)) = (Wrd7.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_1_15]));

DEFLABEL (continuation_1)
  INTERRUPT_CHECK (27, LABEL_1_9);
  if (! (Rvl == ((SCHEME_OBJECT) 0)))
    goto label_14;

DEFLABEL (label_13)
  Rvl = (current_block [OBJECT_1_1]);
  Rsp = (& (Rsp [2]));
  goto pop_return;

DEFLABEL (label_14)
  (Wrd9.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_1_13]))));
  (* (--Rsp)) = (Wrd9.Obj);
  (Wrd10.Obj) = (Rsp [2]);
  (* (--Rsp)) = (Wrd10.Obj);
  (Wrd11.Obj) = (Rsp [2]);
  (Wrd12.pObj) = (OBJECT_ADDRESS (Wrd11.Obj));
  (Wrd13.Obj) = ((Wrd12.pObj) [2]);
  (* (--Rsp)) = (Wrd13.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_1_14]));

DEFLABEL (continuation_2)
  INTERRUPT_CHECK (27, LABEL_1_13);
  if (Rvl == ((SCHEME_OBJECT) 0))
    goto label_13;
  (Wrd20.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_1_12]))));
  (* (--Rsp)) = (Wrd20.Obj);
  (Wrd21.Obj) = (Rsp [2]);
  (* (--Rsp)) = (Wrd21.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_1_11]));

DEFLABEL (continuation_3)
  INTERRUPT_CHECK (27, LABEL_1_12);
  (Wrd26.Obj) = (Rsp [0]);
  (Wrd27.pObj) = (OBJECT_ADDRESS (Wrd26.Obj));
  (Wrd28.Obj) = ((Wrd27.pObj) [4]);
  (Wrd29.pObj) = (OBJECT_ADDRESS (Wrd28.Obj));
  (Wrd30.Obj) = ((Wrd29.pObj) [0]);
  (* (Rhp++)) = Rvl;
  (* (Rhp++)) = (Wrd30.Obj);
  (Wrd25.pObj) = (& (Rhp [-2]));
  (Wrd23.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd25.pObj)));
  ((Wrd29.pObj) [0]) = (Wrd23.Obj);
  goto label_13;

INVOKE_INTERFACE_TARGET_0
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_2_4 3
#define LABEL_2_7 5
#define LABEL_2_8 7
#define LABEL_2_14 9
#define LABEL_2_5 11
#define LABEL_2_9 13
#define LABEL_2_10 15
#define LABEL_2_11 17
#define TAG_2_12 7
#define ENVIRONMENT_LABEL_2_3 36
#define DEBUGGING_LABEL_2_2 35
#define OBJECT_2_1 34
#define OBJECT_2_0 33
#define EXECUTE_CACHE_2_19 19
#define EXECUTE_CACHE_2_18 21
#define EXECUTE_CACHE_2_17 23
#define EXECUTE_CACHE_2_16 25
#define EXECUTE_CACHE_2_15 27
#define EXECUTE_CACHE_2_13 29
#define EXECUTE_CACHE_2_6 31
#define FREE_REFERENCES_LABEL_2_0 18
#define NUMBER_OF_LINKER_SECTIONS_2_1 1

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_2 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  machine_word Wrd5;
  machine_word Wrd6;
  machine_word Wrd15;
  machine_word Wrd14;
  machine_word Wrd13;
  machine_word Wrd20;
  machine_word Wrd37;
  machine_word Wrd36;
  machine_word Wrd34;
  machine_word Wrd35;
  machine_word Wrd33;
  machine_word Wrd30;
  machine_word Wrd27;
  machine_word Wrd41;
  machine_word Wrd42;
  machine_word Wrd9;
  machine_word Wrd8;
  machine_word Wrd7;
  INVOKE_INTERFACE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_2_4);
      goto operator_arglist_12;

    case 1:
      current_block = (Rpc - LABEL_2_7);
      goto continuation_8;

    case 2:
      current_block = (Rpc - LABEL_2_8);
      goto continuation_4;

    case 3:
      current_block = (Rpc - LABEL_2_14);
      goto continuation_3;

    case 4:
      current_block = (Rpc - LABEL_2_5);
      goto continuation_0;

    case 5:
      current_block = (Rpc - LABEL_2_9);
      goto continuation_7;

    case 6:
      current_block = (Rpc - LABEL_2_10);
      goto continuation_6;

    case 7:
      current_block = (Rpc - LABEL_2_11);
      goto lambda_16;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (operator_arglist_15)
DEFLABEL (operator_arglist_12)
  INTERRUPT_CHECK (26, LABEL_2_4);
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_2_5]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd8.Obj) = (Rsp [1]);
  (* (--Rsp)) = (Wrd8.Obj);
  (Wrd9.Obj) = (Rsp [3]);
  (* (--Rsp)) = (Wrd9.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_2_6]));

DEFLABEL (continuation_0)
  INTERRUPT_CHECK (27, LABEL_2_5);
  (* (--Rsp)) = Rvl;
  if (! (Rvl == (current_block [OBJECT_2_1])))
    goto label_20;
  (Wrd13.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_2_14]))));
  (* (--Rsp)) = (Wrd13.Obj);
  (Wrd14.Obj) = (Rsp [2]);
  (* (--Rsp)) = (Wrd14.Obj);
  (Wrd15.Obj) = (Rsp [4]);
  (* (--Rsp)) = (Wrd15.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_2_16]));

DEFLABEL (continuation_3)
  INTERRUPT_CHECK (27, LABEL_2_14);
  (* (--Rsp)) = Rvl;
  if (Rvl == ((SCHEME_OBJECT) 0))
    goto label_19;
  (Wrd20.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_2_8]))));
  (* (--Rsp)) = (Wrd20.Obj);
  (* (--Rsp)) = Rvl;
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_2_15]));

DEFLABEL (continuation_4)
  INTERRUPT_CHECK (27, LABEL_2_8);
  if (Rvl == ((SCHEME_OBJECT) 0))
    goto label_19;
  (Wrd27.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_2_7]))));
  (* (--Rsp)) = (Wrd27.Obj);
  (Wrd30.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_2_9]))));
  (* (--Rsp)) = (Wrd30.Obj);
  (Wrd33.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_2_10]))));
  (* (--Rsp)) = (Wrd33.Obj);
  (* (Rhp++)) = (MAKE_OBJECT (TC_MANIFEST_CLOSURE, 4));
  (Wrd35.pObj) = (Rhp + 1);
  Rhp += 1;
  WRITE_LABEL_DESCRIPTOR (Rhp, 0x101, 2);
  (* (Rhp++)) = (dispatch_base + TAG_2_12);
  (* (Rhp++)) = ((SCHEME_OBJECT) (& (current_block [LABEL_2_11])));
  Rhp += 1;
  (Wrd34.Obj) = (MAKE_POINTER_OBJECT (40, (Wrd35.pObj)));
  Wrd36 = Wrd35;
  (Wrd37.Obj) = (Rsp [3]);
  ((Wrd36.pObj) [2]) = (Wrd37.Obj);
  (* (--Rsp)) = (Wrd34.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_2_13]));

DEFLABEL (continuation_6)
  INTERRUPT_CHECK (27, LABEL_2_10);
  (* (--Rsp)) = Rvl;
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_2_18]));

DEFLABEL (continuation_7)
  INTERRUPT_CHECK (27, LABEL_2_9);
  (* (--Rsp)) = Rvl;
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_2_17]));

DEFLABEL (continuation_8)
  INTERRUPT_CHECK (27, LABEL_2_7);
  (Wrd42.Obj) = (Rsp [2]);
  (* (Rhp++)) = (Wrd42.Obj);
  (* (Rhp++)) = Rvl;
  (Wrd41.pObj) = (& (Rhp [-2]));
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd41.pObj)));

DEFLABEL (label_18)
  Rsp = (& (Rsp [1]));

DEFLABEL (label_17)
  (Rsp [2]) = (Wrd7.Obj);
  Rsp = (& (Rsp [2]));
  Rvl = (Rsp [0]);
  Rsp = (& (Rsp [1]));
  goto pop_return;

DEFLABEL (label_19)
  (Wrd7.Obj) = (current_block [OBJECT_2_0]);
  goto label_18;

DEFLABEL (label_20)
  (Wrd7.Obj) = (current_block [OBJECT_2_0]);
  goto label_17;

DEFLABEL (lambda_16)
  CLOSURE_HEADER (LABEL_2_11);

DEFLABEL (lambda_5)
  CLOSURE_INTERRUPT_CHECK (24);
  (Wrd6.Obj) = (Rsp [0]);
  (Wrd7.pObj) = (OBJECT_ADDRESS (Wrd6.Obj));
  (Wrd5.Obj) = ((Wrd7.pObj) [2]);
  (Rsp [0]) = (Wrd5.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_2_19]));

INVOKE_INTERFACE_TARGET_0
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_3_4 3
#define LABEL_3_5 5
#define LABEL_3_7 7
#define LABEL_3_8 9
#define LABEL_3_9 11
#define LABEL_3_12 13
#define LABEL_3_13 15
#define LABEL_3_14 17
#define LABEL_3_15 19
#define LABEL_3_16 21
#define LABEL_3_17 23
#define LABEL_3_10 25
#define LABEL_3_18 27
#define ENVIRONMENT_LABEL_3_3 48
#define DEBUGGING_LABEL_3_2 47
#define OBJECT_3_13 46
#define OBJECT_3_12 45
#define OBJECT_3_11 44
#define OBJECT_3_10 43
#define OBJECT_3_9 42
#define OBJECT_3_8 41
#define OBJECT_3_7 40
#define OBJECT_3_6 39
#define OBJECT_3_5 38
#define OBJECT_3_4 37
#define OBJECT_3_3 36
#define OBJECT_3_2 35
#define OBJECT_3_1 34
#define OBJECT_3_0 33
#define EXECUTE_CACHE_3_11 29
#define EXECUTE_CACHE_3_6 31
#define FREE_REFERENCES_LABEL_3_0 28
#define NUMBER_OF_LINKER_SECTIONS_3_1 1

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_3 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  machine_word Wrd28;
  machine_word Wrd22;
  machine_word Wrd24;
  machine_word Wrd16;
  machine_word Wrd10;
  machine_word Wrd13;
  machine_word Wrd6;
  machine_word Wrd96;
  machine_word Wrd92;
  machine_word Wrd94;
  machine_word Wrd95;
  machine_word Wrd87;
  machine_word Wrd89;
  machine_word Wrd91;
  machine_word Wrd90;
  machine_word Wrd86;
  machine_word Wrd63;
  machine_word Wrd55;
  machine_word Wrd57;
  machine_word Wrd59;
  machine_word Wrd58;
  machine_word Wrd53;
  machine_word Wrd54;
  machine_word Wrd50;
  machine_word Wrd42;
  machine_word Wrd44;
  machine_word Wrd46;
  machine_word Wrd45;
  machine_word Wrd51;
  machine_word Wrd40;
  machine_word Wrd39;
  machine_word Wrd74;
  machine_word Wrd75;
  machine_word Wrd72;
  machine_word Wrd64;
  machine_word Wrd66;
  machine_word Wrd68;
  machine_word Wrd67;
  machine_word Wrd37;
  machine_word Wrd85;
  machine_word Wrd84;
  machine_word Wrd76;
  machine_word Wrd78;
  machine_word Wrd80;
  machine_word Wrd79;
  machine_word Wrd35;
  machine_word Wrd27;
  machine_word Wrd29;
  machine_word Wrd31;
  machine_word Wrd30;
  machine_word Wrd17;
  machine_word Wrd21;
  machine_word Wrd20;
  machine_word Wrd19;
  machine_word Wrd25;
  machine_word Wrd15;
  machine_word Wrd106;
  machine_word Wrd101;
  machine_word Wrd98;
  machine_word Wrd111;
  machine_word Wrd110;
  machine_word Wrd5;
  machine_word Wrd18;
  machine_word Wrd12;
  machine_word Wrd11;
  machine_word Wrd14;
  machine_word Wrd9;
  machine_word Wrd8;
  machine_word Wrd7;
  INVOKE_INTERFACE_DECLS
  INVOKE_PRIMITIVE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_3_4);
      goto geiser_operator_arglist_31;

    case 1:
      current_block = (Rpc - LABEL_3_5);
      goto continuation_0;

    case 2:
      current_block = (Rpc - LABEL_3_7);
      goto label_33;

    case 3:
      current_block = (Rpc - LABEL_3_8);
      goto continuation_4;

    case 4:
      current_block = (Rpc - LABEL_3_9);
      goto loop_27;

    case 5:
      current_block = (Rpc - LABEL_3_12);
      goto label_39;

    case 6:
      current_block = (Rpc - LABEL_3_13);
      goto label_34;

    case 7:
      current_block = (Rpc - LABEL_3_14);
      goto label_38;

    case 8:
      current_block = (Rpc - LABEL_3_15);
      goto label_37;

    case 9:
      current_block = (Rpc - LABEL_3_16);
      goto label_35;

    case 10:
      current_block = (Rpc - LABEL_3_17);
      goto label_36;

    case 11:
      current_block = (Rpc - LABEL_3_10);
      goto continuation_19;

    case 12:
      current_block = (Rpc - LABEL_3_18);
      goto continuation_17;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (geiser_operator_arglist_41)
DEFLABEL (geiser_operator_arglist_31)
  INTERRUPT_CHECK (26, LABEL_3_4);
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_3_5]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd8.Obj) = (Rsp [2]);
  (* (--Rsp)) = (Wrd8.Obj);
  (Wrd9.Obj) = (Rsp [2]);
  (* (--Rsp)) = (Wrd9.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_3_6]));

DEFLABEL (continuation_0)
  INTERRUPT_CHECK (27, LABEL_3_5);
  (* (--Rsp)) = Rvl;
  if (! (Rvl == ((SCHEME_OBJECT) 0)))
    goto label_43;
  Rvl = (current_block [OBJECT_3_0]);
  Rsp = (& (Rsp [3]));
  goto pop_return;

DEFLABEL (label_43)
  (Wrd7.Obj) = (current_block [OBJECT_3_0]);
  (* (--Rsp)) = (Wrd7.Obj);
  (* (--Rsp)) = (Wrd7.Obj);
  (* (--Rsp)) = ((SCHEME_OBJECT) 0);
  (Wrd14.uLng) = (OBJECT_TYPE (Rvl));
  if (! ((Wrd14.uLng) == 1))
    goto label_45;
  (Wrd11.pObj) = (OBJECT_ADDRESS (Rvl));
  (Wrd12.Obj) = ((Wrd11.pObj) [1]);
  (* (--Rsp)) = (Wrd12.Obj);

DEFLABEL (label_44)
  goto loop_27;

DEFLABEL (label_45)
  (Wrd18.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_3_7]))));
  (* (--Rsp)) = (Wrd18.Obj);
  (* (--Rsp)) = Rvl;
  INVOKE_PRIMITIVE ((current_block [OBJECT_3_1]), 1);

DEFLABEL (label_33)
  (* (--Rsp)) = Rvl;
  goto label_44;

DEFLABEL (loop_42)
DEFLABEL (loop_27)
  INTERRUPT_CHECK (26, LABEL_3_9);
  (Wrd5.Obj) = (Rsp [0]);
  if (! ((Wrd5.Obj) == (current_block [OBJECT_3_0])))
    goto label_46;
  (Wrd110.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_3_10]))));
  (* (--Rsp)) = (Wrd110.Obj);
  (Wrd111.Obj) = (Rsp [4]);
  (* (--Rsp)) = (Wrd111.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_3_11]));

DEFLABEL (continuation_19)
  INTERRUPT_CHECK (27, LABEL_3_10);
  (Wrd9.Obj) = (current_block [OBJECT_3_10]);
  (* (Rhp++)) = (Wrd9.Obj);
  (* (Rhp++)) = Rvl;
  (Wrd8.pObj) = (& (Rhp [-2]));
  (Wrd6.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd8.pObj)));
  (Wrd13.Obj) = (current_block [OBJECT_3_11]);
  (Wrd14.Obj) = (current_block [OBJECT_3_0]);
  (* (Rhp++)) = (Wrd13.Obj);
  (* (Rhp++)) = (Wrd14.Obj);
  (Wrd12.pObj) = (& (Rhp [-2]));
  (Wrd10.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd12.pObj)));
  (* (Rhp++)) = (Wrd6.Obj);
  (* (Rhp++)) = (Wrd10.Obj);
  (Wrd16.pObj) = (& (Rhp [-2]));
  (Wrd17.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd16.pObj)));
  (* (--Rsp)) = (Wrd17.Obj);
  (Wrd20.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_3_18]))));
  (* (--Rsp)) = (Wrd20.Obj);
  (Wrd21.Obj) = (Rsp [4]);
  (* (--Rsp)) = (Wrd21.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_3_11]));

DEFLABEL (continuation_17)
  INTERRUPT_CHECK (27, LABEL_3_18);
  (Wrd9.Obj) = (current_block [OBJECT_3_12]);
  (* (Rhp++)) = (Wrd9.Obj);
  (* (Rhp++)) = Rvl;
  (Wrd8.pObj) = (& (Rhp [-2]));
  (Wrd6.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd8.pObj)));
  (Wrd10.Obj) = (* (Rsp++));
  (* (Rhp++)) = (Wrd6.Obj);
  (* (Rhp++)) = (Wrd10.Obj);
  (Wrd13.pObj) = (& (Rhp [-2]));
  (Wrd11.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd13.pObj)));
  (Wrd17.Obj) = (current_block [OBJECT_3_0]);
  (* (Rhp++)) = (Wrd11.Obj);
  (* (Rhp++)) = (Wrd17.Obj);
  (Wrd16.pObj) = (& (Rhp [-2]));
  (Wrd14.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd16.pObj)));
  (Wrd21.Obj) = (current_block [OBJECT_3_13]);
  (* (Rhp++)) = (Wrd21.Obj);
  (* (Rhp++)) = (Wrd14.Obj);
  (Wrd20.pObj) = (& (Rhp [-2]));
  (Wrd18.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd20.pObj)));
  (* (Rhp++)) = (Wrd18.Obj);
  (* (Rhp++)) = (Wrd17.Obj);
  (Wrd24.pObj) = (& (Rhp [-2]));
  (Wrd22.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd24.pObj)));
  (Wrd30.Obj) = (Rsp [5]);
  (* (Rhp++)) = (Wrd30.Obj);
  (* (Rhp++)) = (Wrd22.Obj);
  (Wrd28.pObj) = (& (Rhp [-2]));
  Rvl = (MAKE_POINTER_OBJECT (1, (Wrd28.pObj)));
  Rsp = (& (Rsp [7]));
  goto pop_return;

DEFLABEL (label_46)
  (Wrd9.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_3_8]))));
  (* (--Rsp)) = (Wrd9.Obj);
  (* (--Rsp)) = (Wrd5.Obj);
  (Wrd98.Obj) = (current_block [OBJECT_3_2]);
  (Wrd101.Lng) = (1 << TYPE_CODE_LENGTH);
  if (! (((unsigned long) 29L) < ((unsigned long) (Wrd101.Lng))))
    goto label_70;
  (Wrd12.uLng) = (OBJECT_DATUM (Wrd98.Obj));
  (Wrd14.uLng) = (OBJECT_TYPE (Wrd5.Obj));
  if ((Wrd12.Obj) == (Wrd14.Obj))
    goto label_69;

DEFLABEL (label_68)
  (Wrd15.Obj) = (current_block [OBJECT_3_4]);
  (* (--Rsp)) = (Wrd15.Obj);
  (Wrd25.Lng) = (1 << TYPE_CODE_LENGTH);
  if (((unsigned long) 5L) < ((unsigned long) (Wrd25.Lng)))
    goto label_47;
  INVOKE_PRIMITIVE ((current_block [OBJECT_3_3]), 2);

DEFLABEL (label_47)
  (Wrd19.uLng) = (OBJECT_DATUM (Wrd15.Obj));
  (Wrd20.Obj) = (Rsp [1]);
  (Wrd21.uLng) = (OBJECT_TYPE (Wrd20.Obj));
  if ((Wrd19.Obj) == (Wrd21.Obj))
    goto label_49;
  (Wrd17.Obj) = (current_block [OBJECT_3_6]);
  goto label_48;

DEFLABEL (label_49)
  (Wrd17.Obj) = (current_block [OBJECT_3_5]);

DEFLABEL (label_48)
DEFLABEL (label_67)
  Rsp = (& (Rsp [3]));
  if ((Wrd17.Obj) == ((SCHEME_OBJECT) 0))
    goto label_51;

DEFLABEL (label_50)
  (Wrd86.Obj) = (current_block [OBJECT_3_5]);
  (Rsp [1]) = (Wrd86.Obj);
  (Wrd90.Obj) = (Rsp [0]);
  (Wrd91.Obj) = (Rsp [3]);
  (* (Rhp++)) = (Wrd90.Obj);
  (* (Rhp++)) = (Wrd91.Obj);
  (Wrd89.pObj) = (& (Rhp [-2]));
  (Wrd87.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd89.pObj)));
  (Wrd95.Obj) = (current_block [OBJECT_3_9]);
  (* (Rhp++)) = (Wrd95.Obj);
  (* (Rhp++)) = (Wrd87.Obj);
  (Wrd94.pObj) = (& (Rhp [-2]));
  (Wrd92.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd94.pObj)));
  (Rsp [3]) = (Wrd92.Obj);
  (Wrd96.Obj) = (current_block [OBJECT_3_0]);
  (Rsp [0]) = (Wrd96.Obj);
  goto loop_27;

DEFLABEL (label_51)
  (Wrd30.Obj) = (Rsp [0]);
  (Wrd31.uLng) = (OBJECT_TYPE (Wrd30.Obj));
  if (! ((Wrd31.uLng) == 1))
    goto label_66;
  (Wrd29.pObj) = (OBJECT_ADDRESS (Wrd30.Obj));
  (Wrd27.Obj) = ((Wrd29.pObj) [0]);

DEFLABEL (label_65)
  if ((Wrd27.Obj) == (current_block [OBJECT_3_8]))
    goto label_62;
  (Wrd37.Obj) = (Rsp [1]);
  if ((Wrd37.Obj) == ((SCHEME_OBJECT) 0))
    goto label_59;
  (Wrd39.Obj) = (Rsp [2]);

DEFLABEL (label_58)
  (Rsp [2]) = (Wrd39.Obj);
  (Wrd40.Obj) = (Rsp [1]);
  if ((Wrd40.Obj) == ((SCHEME_OBJECT) 0))
    goto label_57;
  (Wrd45.Obj) = (Rsp [0]);
  (Wrd46.uLng) = (OBJECT_TYPE (Wrd45.Obj));
  if (! ((Wrd46.uLng) == 1))
    goto label_56;
  (Wrd44.pObj) = (OBJECT_ADDRESS (Wrd45.Obj));
  (Wrd42.Obj) = ((Wrd44.pObj) [0]);

DEFLABEL (label_55)
  (Wrd54.Obj) = (Rsp [3]);
  (* (Rhp++)) = (Wrd42.Obj);
  (* (Rhp++)) = (Wrd54.Obj);
  (Wrd53.pObj) = (& (Rhp [-2]));
  (Wrd51.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd53.pObj)));

DEFLABEL (label_54)
  (Rsp [3]) = (Wrd51.Obj);
  (Wrd58.Obj) = (Rsp [0]);
  (Wrd59.uLng) = (OBJECT_TYPE (Wrd58.Obj));
  if (! ((Wrd59.uLng) == 1))
    goto label_53;
  (Wrd57.pObj) = (OBJECT_ADDRESS (Wrd58.Obj));
  (Wrd55.Obj) = ((Wrd57.pObj) [1]);

DEFLABEL (label_52)
  (Rsp [0]) = (Wrd55.Obj);
  goto loop_27;

DEFLABEL (label_53)
  (Wrd63.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_3_17]))));
  (* (--Rsp)) = (Wrd63.Obj);
  (* (--Rsp)) = (Wrd58.Obj);
  INVOKE_PRIMITIVE ((current_block [OBJECT_3_1]), 1);

DEFLABEL (label_36)
  (Wrd55.Obj) = Rvl;
  goto label_52;

DEFLABEL (label_56)
  (Wrd50.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_3_16]))));
  (* (--Rsp)) = (Wrd50.Obj);
  (* (--Rsp)) = (Wrd45.Obj);
  INVOKE_PRIMITIVE ((current_block [OBJECT_3_7]), 1);

DEFLABEL (label_35)
  (Wrd42.Obj) = Rvl;
  goto label_55;

DEFLABEL (label_57)
  (Wrd51.Obj) = (Rsp [3]);
  goto label_54;

DEFLABEL (label_59)
  (Wrd67.Obj) = (Rsp [0]);
  (Wrd68.uLng) = (OBJECT_TYPE (Wrd67.Obj));
  if (! ((Wrd68.uLng) == 1))
    goto label_61;
  (Wrd66.pObj) = (OBJECT_ADDRESS (Wrd67.Obj));
  (Wrd64.Obj) = ((Wrd66.pObj) [0]);

DEFLABEL (label_60)
  (Wrd75.Obj) = (Rsp [2]);
  (* (Rhp++)) = (Wrd64.Obj);
  (* (Rhp++)) = (Wrd75.Obj);
  (Wrd74.pObj) = (& (Rhp [-2]));
  (Wrd39.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd74.pObj)));
  goto label_58;

DEFLABEL (label_61)
  (Wrd72.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_3_15]))));
  (* (--Rsp)) = (Wrd72.Obj);
  (* (--Rsp)) = (Wrd67.Obj);
  INVOKE_PRIMITIVE ((current_block [OBJECT_3_7]), 1);

DEFLABEL (label_37)
  (Wrd64.Obj) = Rvl;
  goto label_60;

DEFLABEL (label_62)
  (Wrd79.Obj) = (Rsp [0]);
  (Wrd80.uLng) = (OBJECT_TYPE (Wrd79.Obj));
  if (! ((Wrd80.uLng) == 1))
    goto label_64;
  (Wrd78.pObj) = (OBJECT_ADDRESS (Wrd79.Obj));
  (Wrd76.Obj) = ((Wrd78.pObj) [1]);

DEFLABEL (label_63)
  (Rsp [0]) = (Wrd76.Obj);
  (Wrd85.Obj) = (current_block [OBJECT_3_5]);
  (Rsp [1]) = (Wrd85.Obj);
  goto loop_27;

DEFLABEL (label_64)
  (Wrd84.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_3_14]))));
  (* (--Rsp)) = (Wrd84.Obj);
  (* (--Rsp)) = (Wrd79.Obj);
  INVOKE_PRIMITIVE ((current_block [OBJECT_3_1]), 1);

DEFLABEL (label_38)
  (Wrd76.Obj) = Rvl;
  goto label_63;

DEFLABEL (label_66)
  (Wrd35.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_3_13]))));
  (* (--Rsp)) = (Wrd35.Obj);
  (* (--Rsp)) = (Wrd30.Obj);
  INVOKE_PRIMITIVE ((current_block [OBJECT_3_7]), 1);

DEFLABEL (label_34)
  (Wrd27.Obj) = Rvl;
  goto label_65;

DEFLABEL (label_69)
  Rsp = (& (Rsp [2]));
  goto label_50;

DEFLABEL (label_70)
  (Wrd106.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_3_12]))));
  (* (--Rsp)) = (Wrd106.Obj);
  (* (--Rsp)) = (Wrd5.Obj);
  (* (--Rsp)) = (Wrd98.Obj);
  INVOKE_PRIMITIVE ((current_block [OBJECT_3_3]), 2);

DEFLABEL (label_39)
  if (Rvl == ((SCHEME_OBJECT) 0))
    goto label_68;
  goto label_69;

DEFLABEL (continuation_4)
  INTERRUPT_CHECK (27, LABEL_3_8);
  if (Rvl == ((SCHEME_OBJECT) 0))
    goto label_51;
  goto label_50;

INVOKE_INTERFACE_TARGET_1
INVOKE_PRIMITIVE_TARGET
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_4_4 3
#define LABEL_4_5 5
#define ENVIRONMENT_LABEL_4_3 12
#define DEBUGGING_LABEL_4_2 11
#define EXECUTE_CACHE_4_6 7
#define FREE_REFERENCE_4_0 10
#define FREE_REFERENCES_LABEL_4_0 6
#define NUMBER_OF_LINKER_SECTIONS_4_1 2

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_4 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  machine_word Wrd7;
  machine_word Wrd12;
  machine_word Wrd11;
  machine_word Wrd8;
  machine_word Wrd5;
  INVOKE_INTERFACE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_4_4);
      goto read_from_string_0;

    case 1:
      current_block = (Rpc - LABEL_4_5);
      goto label_2;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (read_from_string_4)
DEFLABEL (read_from_string_0)
  INTERRUPT_CHECK (26, LABEL_4_4);
  (Wrd5.Obj) = (Rsp [0]);
  (* (--Rsp)) = (Wrd5.Obj);
  (Wrd8.pObj) = ((SCHEME_OBJECT *) (current_block [FREE_REFERENCE_4_0]));
  (Wrd11.Obj) = ((Wrd8.pObj) [0]);
  (Wrd12.uLng) = (OBJECT_TYPE (Wrd11.Obj));
  if ((Wrd12.uLng) == 50)
    goto label_6;
  Wrd7 = Wrd11;

DEFLABEL (label_5)
  (Rsp [1]) = (Wrd7.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_4_6]));

DEFLABEL (label_6)
  INVOKE_INTERFACE_2 (31, (& (current_block [LABEL_4_5])), (Wrd8.pObj));

DEFLABEL (label_2)
  (Wrd7.Obj) = Rvl;
  goto label_5;

INVOKE_INTERFACE_TARGET_1
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_5_4 3
#define LABEL_5_5 5
#define LABEL_5_7 7
#define LABEL_5_8 9
#define LABEL_5_9 11
#define ENVIRONMENT_LABEL_5_3 21
#define DEBUGGING_LABEL_5_2 20
#define OBJECT_5_0 19
#define EXECUTE_CACHE_5_11 13
#define EXECUTE_CACHE_5_10 15
#define EXECUTE_CACHE_5_6 17
#define FREE_REFERENCES_LABEL_5_0 12
#define NUMBER_OF_LINKER_SECTIONS_5_1 1

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_5 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  machine_word Wrd6;
  machine_word Wrd11;
  machine_word Wrd10;
  machine_word Wrd8;
  machine_word Wrd7;
  INVOKE_INTERFACE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_5_4);
      goto all_packages_6;

    case 1:
      current_block = (Rpc - LABEL_5_5);
      goto continuation_0;

    case 2:
      current_block = (Rpc - LABEL_5_7);
      goto loop_4;

    case 3:
      current_block = (Rpc - LABEL_5_8);
      goto continuation_3;

    case 4:
      current_block = (Rpc - LABEL_5_9);
      goto continuation_2;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (all_packages_9)
DEFLABEL (all_packages_6)
  INTERRUPT_CHECK (26, LABEL_5_4);
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_5_5]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd8.Obj) = (current_block [OBJECT_5_0]);
  (* (--Rsp)) = (Wrd8.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_5_6]));

DEFLABEL (continuation_0)
  INTERRUPT_CHECK (27, LABEL_5_5);
  (* (--Rsp)) = Rvl;
  goto loop_4;

DEFLABEL (loop_10)
DEFLABEL (loop_4)
  INTERRUPT_CHECK (26, LABEL_5_7);
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_5_8]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd10.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_5_9]))));
  (* (--Rsp)) = (Wrd10.Obj);
  (Wrd11.Obj) = (Rsp [2]);
  (* (--Rsp)) = (Wrd11.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_5_10]));

DEFLABEL (continuation_2)
  INTERRUPT_CHECK (27, LABEL_5_9);
  (* (--Rsp)) = Rvl;
  (Wrd6.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_5_7]))));
  (* (--Rsp)) = (Wrd6.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_5_11]));

DEFLABEL (continuation_3)
  INTERRUPT_CHECK (27, LABEL_5_8);
  (Wrd10.Obj) = (Rsp [0]);
  (* (Rhp++)) = (Wrd10.Obj);
  (* (Rhp++)) = Rvl;
  (Wrd8.pObj) = (& (Rhp [-2]));
  Rvl = (MAKE_POINTER_OBJECT (1, (Wrd8.pObj)));
  Rsp = (& (Rsp [1]));
  goto pop_return;

INVOKE_INTERFACE_TARGET_1
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_6_4 3
#define LABEL_6_5 5
#define LABEL_6_7 7
#define LABEL_6_10 9
#define LABEL_6_8 11
#define ENVIRONMENT_LABEL_6_3 26
#define DEBUGGING_LABEL_6_2 25
#define EXECUTE_CACHE_6_13 13
#define EXECUTE_CACHE_6_12 15
#define EXECUTE_CACHE_6_11 17
#define EXECUTE_CACHE_6_9 19
#define EXECUTE_CACHE_6_6 21
#define FREE_REFERENCE_6_0 24
#define FREE_REFERENCES_LABEL_6_0 12
#define NUMBER_OF_LINKER_SECTIONS_6_1 2

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_6 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  machine_word Wrd9;
  machine_word Wrd21;
  machine_word Wrd20;
  machine_word Wrd12;
  machine_word Wrd17;
  machine_word Wrd16;
  machine_word Wrd13;
  machine_word Wrd8;
  machine_word Wrd7;
  INVOKE_INTERFACE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_6_4);
      goto env__pstring_4;

    case 1:
      current_block = (Rpc - LABEL_6_5);
      goto continuation_0;

    case 2:
      current_block = (Rpc - LABEL_6_7);
      goto label_6;

    case 3:
      current_block = (Rpc - LABEL_6_10);
      goto continuation_2;

    case 4:
      current_block = (Rpc - LABEL_6_8);
      goto continuation_1;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (env__pstring_8)
DEFLABEL (env__pstring_4)
  INTERRUPT_CHECK (26, LABEL_6_4);
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_6_5]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd8.Obj) = (Rsp [1]);
  (* (--Rsp)) = (Wrd8.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_6_6]));

DEFLABEL (continuation_0)
  INTERRUPT_CHECK (27, LABEL_6_5);
  (* (--Rsp)) = Rvl;
  if (Rvl == ((SCHEME_OBJECT) 0))
    goto label_9;
  (Wrd9.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_6_10]))));
  (* (--Rsp)) = (Wrd9.Obj);
  (* (--Rsp)) = Rvl;
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_6_11]));

DEFLABEL (continuation_2)
  INTERRUPT_CHECK (27, LABEL_6_10);
  (Rsp [1]) = Rvl;
  Rsp = (& (Rsp [1]));
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_6_12]));

DEFLABEL (label_9)
  (Wrd13.pObj) = ((SCHEME_OBJECT *) (current_block [FREE_REFERENCE_6_0]));
  (Wrd16.Obj) = ((Wrd13.pObj) [0]);
  (Wrd17.uLng) = (OBJECT_TYPE (Wrd16.Obj));
  if ((Wrd17.uLng) == 50)
    goto label_11;
  Wrd12 = Wrd16;

DEFLABEL (label_10)
  (Rsp [0]) = (Wrd12.Obj);
  (Wrd20.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_6_8]))));
  (* (--Rsp)) = (Wrd20.Obj);
  (Wrd21.Obj) = (Rsp [2]);
  (* (--Rsp)) = (Wrd21.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_6_9]));

DEFLABEL (continuation_1)
  INTERRUPT_CHECK (27, LABEL_6_8);
  (Rsp [1]) = Rvl;
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_6_13]));

DEFLABEL (label_11)
  INVOKE_INTERFACE_2 (31, (& (current_block [LABEL_6_7])), (Wrd13.pObj));

DEFLABEL (label_6)
  (Wrd12.Obj) = Rvl;
  goto label_10;

INVOKE_INTERFACE_TARGET_1
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_7_4 3
#define LABEL_7_5 5
#define LABEL_7_6 7
#define LABEL_7_10 9
#define LABEL_7_7 11
#define LABEL_7_8 13
#define ENVIRONMENT_LABEL_7_3 27
#define DEBUGGING_LABEL_7_2 26
#define OBJECT_7_0 25
#define EXECUTE_CACHE_7_13 15
#define EXECUTE_CACHE_7_12 17
#define EXECUTE_CACHE_7_11 19
#define EXECUTE_CACHE_7_9 21
#define FREE_REFERENCE_7_0 24
#define FREE_REFERENCES_LABEL_7_0 14
#define NUMBER_OF_LINKER_SECTIONS_7_1 2

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_7 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  machine_word Wrd5;
  machine_word Wrd9;
  machine_word Wrd6;
  machine_word Wrd17;
  machine_word Wrd16;
  machine_word Wrd13;
  machine_word Wrd10;
  machine_word Wrd7;
  INVOKE_INTERFACE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_7_4);
      goto set_geiser_repl_promptB_4;

    case 1:
      current_block = (Rpc - LABEL_7_5);
      goto continuation_3;

    case 2:
      current_block = (Rpc - LABEL_7_6);
      goto continuation_2;

    case 3:
      current_block = (Rpc - LABEL_7_10);
      goto label_6;

    case 4:
      current_block = (Rpc - LABEL_7_7);
      goto continuation_1;

    case 5:
      current_block = (Rpc - LABEL_7_8);
      goto continuation_0;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (set_geiser_repl_promptB_8)
DEFLABEL (set_geiser_repl_promptB_4)
  INTERRUPT_CHECK (26, LABEL_7_4);
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_7_5]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd10.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_7_6]))));
  (* (--Rsp)) = (Wrd10.Obj);
  (Wrd13.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_7_7]))));
  (* (--Rsp)) = (Wrd13.Obj);
  (Wrd16.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_7_8]))));
  (* (--Rsp)) = (Wrd16.Obj);
  (Wrd17.Obj) = (Rsp [4]);
  (* (--Rsp)) = (Wrd17.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_7_9]));

DEFLABEL (continuation_0)
  INTERRUPT_CHECK (27, LABEL_7_8);
  (* (--Rsp)) = Rvl;
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_7_13]));

DEFLABEL (continuation_1)
  INTERRUPT_CHECK (27, LABEL_7_7);
  (* (--Rsp)) = Rvl;
  (Wrd5.Obj) = (current_block [OBJECT_7_0]);
  (* (--Rsp)) = (Wrd5.Obj);
  (* (--Rsp)) = ((SCHEME_OBJECT) 0);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_7_12]));

DEFLABEL (continuation_2)
  INTERRUPT_CHECK (27, LABEL_7_6);
  (* (--Rsp)) = Rvl;
  (Wrd6.pObj) = ((SCHEME_OBJECT *) (current_block [FREE_REFERENCE_7_0]));
  (Wrd9.Obj) = ((Wrd6.pObj) [0]);
  (Wrd10.uLng) = (OBJECT_TYPE (Wrd9.Obj));
  if ((Wrd10.uLng) == 50)
    goto label_10;
  Wrd5 = Wrd9;

DEFLABEL (label_9)
  (* (--Rsp)) = (Wrd5.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_7_11]));

DEFLABEL (continuation_3)
  INTERRUPT_CHECK (27, LABEL_7_5);
  Rvl = (Rsp [0]);
  Rsp = (& (Rsp [1]));
  goto pop_return;

DEFLABEL (label_10)
  INVOKE_INTERFACE_2 (31, (& (current_block [LABEL_7_10])), (Wrd6.pObj));

DEFLABEL (label_6)
  (Wrd5.Obj) = Rvl;
  goto label_9;

INVOKE_INTERFACE_TARGET_1
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_8_4 3
#define LABEL_8_8 5
#define LABEL_8_12 7
#define LABEL_8_14 9
#define LABEL_8_15 11
#define LABEL_8_17 13
#define LABEL_8_5 15
#define LABEL_8_20 17
#define LABEL_8_6 19
#define LABEL_8_13 21
#define LABEL_8_21 23
#define LABEL_8_22 25
#define LABEL_8_23 27
#define LABEL_8_9 29
#define LABEL_8_10 31
#define ENVIRONMENT_LABEL_8_3 58
#define DEBUGGING_LABEL_8_2 57
#define OBJECT_8_5 56
#define OBJECT_8_4 55
#define OBJECT_8_3 54
#define OBJECT_8_2 53
#define OBJECT_8_1 52
#define OBJECT_8_0 51
#define EXECUTE_CACHE_8_27 33
#define EXECUTE_CACHE_8_26 35
#define EXECUTE_CACHE_8_25 37
#define EXECUTE_CACHE_8_24 39
#define EXECUTE_CACHE_8_19 41
#define EXECUTE_CACHE_8_18 43
#define EXECUTE_CACHE_8_16 45
#define EXECUTE_CACHE_8_11 47
#define EXECUTE_CACHE_8_7 49
#define FREE_REFERENCES_LABEL_8_0 32
#define NUMBER_OF_LINKER_SECTIONS_8_1 1

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_8 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  machine_word Wrd9;
  machine_word Wrd14;
  machine_word Wrd13;
  machine_word Wrd25;
  machine_word Wrd21;
  machine_word Wrd18;
  machine_word Wrd17;
  machine_word Wrd15;
  machine_word Wrd8;
  machine_word Wrd12;
  machine_word Wrd47;
  machine_word Wrd46;
  machine_word Wrd39;
  machine_word Wrd36;
  machine_word Wrd45;
  machine_word Wrd41;
  machine_word Wrd42;
  machine_word Wrd40;
  machine_word Wrd34;
  machine_word Wrd24;
  machine_word Wrd28;
  machine_word Wrd27;
  machine_word Wrd30;
  machine_word Wrd29;
  machine_word Wrd23;
  machine_word Wrd20;
  machine_word Wrd55;
  machine_word Wrd54;
  machine_word Wrd51;
  machine_word Wrd5;
  machine_word Wrd11;
  machine_word Wrd10;
  machine_word Wrd7;
  INVOKE_INTERFACE_DECLS
  INVOKE_PRIMITIVE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_8_4);
      goto get_symbol_definition_location_16;

    case 1:
      current_block = (Rpc - LABEL_8_8);
      goto continuation_3;

    case 2:
      current_block = (Rpc - LABEL_8_12);
      goto continuation_10;

    case 3:
      current_block = (Rpc - LABEL_8_14);
      goto label_18;

    case 4:
      current_block = (Rpc - LABEL_8_15);
      goto label_19;

    case 5:
      current_block = (Rpc - LABEL_8_17);
      goto continuation_11;

    case 6:
      current_block = (Rpc - LABEL_8_5);
      goto continuation_2;

    case 7:
      current_block = (Rpc - LABEL_8_20);
      goto continuation_1;

    case 8:
      current_block = (Rpc - LABEL_8_6);
      goto continuation_0;

    case 9:
      current_block = (Rpc - LABEL_8_13);
      goto continuation_14;

    case 10:
      current_block = (Rpc - LABEL_8_21);
      goto lambda_6;

    case 11:
      current_block = (Rpc - LABEL_8_22);
      goto continuation_8;

    case 12:
      current_block = (Rpc - LABEL_8_23);
      goto continuation_7;

    case 13:
      current_block = (Rpc - LABEL_8_9);
      goto lambda_4;

    case 14:
      current_block = (Rpc - LABEL_8_10);
      goto continuation_5;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (get_symbol_definition_location_21)
DEFLABEL (get_symbol_definition_location_16)
  INTERRUPT_CHECK (26, LABEL_8_4);
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_8_5]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd10.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_8_6]))));
  (* (--Rsp)) = (Wrd10.Obj);
  (Wrd11.Obj) = (Rsp [2]);
  (* (--Rsp)) = (Wrd11.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_8_7]));

DEFLABEL (continuation_0)
  INTERRUPT_CHECK (27, LABEL_8_6);
  if (Rvl == ((SCHEME_OBJECT) 0))
    goto label_31;
  (Wrd9.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_8_20]))));
  (* (--Rsp)) = (Wrd9.Obj);
  (Wrd10.Obj) = (Rsp [2]);
  (* (--Rsp)) = (Wrd10.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_8_26]));

DEFLABEL (continuation_1)
  INTERRUPT_CHECK (27, LABEL_8_20);
  if (Rvl == ((SCHEME_OBJECT) 0))
    goto label_31;
  (Wrd18.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_8_21]))));
  (* (--Rsp)) = (Wrd18.Obj);
  (Wrd21.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_8_22]))));
  (* (--Rsp)) = (Wrd21.Obj);
  (Wrd24.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_8_23]))));
  (* (--Rsp)) = (Wrd24.Obj);
  (Wrd25.Obj) = (Rsp [4]);
  (* (--Rsp)) = (Wrd25.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_8_24]));

DEFLABEL (continuation_7)
  INTERRUPT_CHECK (27, LABEL_8_23);
  (* (--Rsp)) = Rvl;
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_8_11]));

DEFLABEL (continuation_8)
  INTERRUPT_CHECK (27, LABEL_8_22);
  (* (--Rsp)) = Rvl;
  {
    SCHEME_OBJECT procedure = (* (Rsp++));
    INVOKE_INTERFACE_2 (20, procedure, 2);
  }

DEFLABEL (continuation_2)
  INTERRUPT_CHECK (27, LABEL_8_5);
  (Wrd5.Obj) = Rvl;

DEFLABEL (label_30)
  (Rsp [0]) = (Wrd5.Obj);
  (Wrd8.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_8_17]))));
  (* (--Rsp)) = (Wrd8.Obj);
  (Wrd10.uLng) = (OBJECT_TYPE (Wrd5.Obj));
  if (! ((Wrd10.uLng) == 30))
    goto label_29;
  (Wrd15.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_8_12]))));
  (* (--Rsp)) = (Wrd15.Obj);
  (* (--Rsp)) = (Wrd5.Obj);
  (Wrd17.Obj) = (current_block [OBJECT_8_5]);
  (* (--Rsp)) = (Wrd17.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_8_18]));

DEFLABEL (continuation_10)
  INTERRUPT_CHECK (27, LABEL_8_12);
  if (Rvl == ((SCHEME_OBJECT) 0))
    goto label_29;
  (Wrd20.Obj) = (current_block [OBJECT_8_1]);
  (* (--Rsp)) = (Wrd20.Obj);
  (Wrd23.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_8_13]))));
  (* (--Rsp)) = (Wrd23.Obj);
  (Wrd29.Obj) = (Rsp [3]);
  (Wrd30.uLng) = (OBJECT_TYPE (Wrd29.Obj));
  if (! ((Wrd30.uLng) == 30))
    goto label_28;
  (Wrd27.pObj) = (OBJECT_ADDRESS (Wrd29.Obj));
  (Wrd28.Obj) = ((Wrd27.pObj) [1]);
  (Wrd24.Obj) = (MAKE_OBJECT (26, (Wrd28.uLng)));

DEFLABEL (label_27)
  (Wrd40.uLng) = (OBJECT_TYPE (Wrd24.Obj));
  if (! ((Wrd40.uLng) == 26))
    goto label_26;
  (Wrd42.Lng) = (FIXNUM_TO_LONG (Wrd24.Obj));
  (Wrd41.Lng) = ((Wrd42.Lng) - 3L);
  if (! (LONG_TO_FIXNUM_P (Wrd41.Lng)))
    goto label_26;
  (Wrd45.Obj) = (LONG_TO_FIXNUM (Wrd41.Lng));
  (* (--Rsp)) = (Wrd45.Obj);

DEFLABEL (label_25)
  (Wrd46.Obj) = (current_block [OBJECT_8_4]);
  (* (--Rsp)) = (Wrd46.Obj);
  (Wrd47.Obj) = (Rsp [5]);
  (* (--Rsp)) = (Wrd47.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_8_16]));

DEFLABEL (continuation_14)
  INTERRUPT_CHECK (27, LABEL_8_13);
  (* (--Rsp)) = Rvl;
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_8_27]));

DEFLABEL (continuation_11)
  INTERRUPT_CHECK (27, LABEL_8_17);
  (Wrd12.Obj) = Rvl;

DEFLABEL (label_24)
  (Rsp [0]) = (Wrd12.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_8_19]));

DEFLABEL (label_26)
  (Wrd36.Obj) = (current_block [OBJECT_8_3]);
  (Wrd39.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_8_15]))));
  (* (--Rsp)) = (Wrd39.Obj);
  (* (--Rsp)) = (Wrd36.Obj);
  (* (--Rsp)) = (Wrd24.Obj);
  INVOKE_INTERFACE_0 (40);

DEFLABEL (label_19)
  (* (--Rsp)) = Rvl;
  goto label_25;

DEFLABEL (label_28)
  (Wrd34.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_8_14]))));
  (* (--Rsp)) = (Wrd34.Obj);
  (* (--Rsp)) = (Wrd29.Obj);
  INVOKE_PRIMITIVE ((current_block [OBJECT_8_2]), 1);

DEFLABEL (label_18)
  (Wrd24.Obj) = Rvl;
  goto label_27;

DEFLABEL (label_29)
  (Wrd12.Obj) = (Rsp [1]);
  Rsp = (& (Rsp [1]));
  goto label_24;

DEFLABEL (label_31)
  (Wrd13.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_8_8]))));
  (* (--Rsp)) = (Wrd13.Obj);
  (Wrd14.Obj) = (Rsp [2]);
  (* (--Rsp)) = (Wrd14.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_8_25]));

DEFLABEL (continuation_3)
  INTERRUPT_CHECK (27, LABEL_8_8);
  if (Rvl == ((SCHEME_OBJECT) 0))
    goto label_32;
  (Wrd51.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_8_9]))));
  (* (--Rsp)) = (Wrd51.Obj);
  (Wrd54.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_8_10]))));
  (* (--Rsp)) = (Wrd54.Obj);
  (Wrd55.Obj) = (Rsp [3]);
  (* (--Rsp)) = (Wrd55.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_8_11]));

DEFLABEL (continuation_5)
  INTERRUPT_CHECK (27, LABEL_8_10);
  (* (--Rsp)) = Rvl;
  {
    SCHEME_OBJECT procedure = (* (Rsp++));
    INVOKE_INTERFACE_2 (20, procedure, 2);
  }

DEFLABEL (label_32)
  (Wrd5.Obj) = (current_block [OBJECT_8_0]);
  Rsp = (& (Rsp [1]));
  goto label_30;

DEFLABEL (lambda_22)
DEFLABEL (lambda_6)
  INTERRUPT_CHECK (26, LABEL_8_21);
  Rvl = (Rsp [0]);
  Rsp = (& (Rsp [2]));
  goto pop_return;

DEFLABEL (lambda_23)
DEFLABEL (lambda_4)
  INTERRUPT_CHECK (26, LABEL_8_9);
  Rvl = (Rsp [0]);
  Rsp = (& (Rsp [2]));
  goto pop_return;

INVOKE_INTERFACE_TARGET_0
INVOKE_PRIMITIVE_TARGET
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_9_5 3
#define LABEL_9_4 5
#define LABEL_9_8 7
#define LABEL_9_9 9
#define LABEL_9_6 11
#define LABEL_9_11 13
#define LABEL_9_13 15
#define LABEL_9_12 17
#define ENVIRONMENT_LABEL_9_3 33
#define DEBUGGING_LABEL_9_2 32
#define OBJECT_9_2 31
#define OBJECT_9_1 30
#define OBJECT_9_0 29
#define EXECUTE_CACHE_9_15 19
#define EXECUTE_CACHE_9_14 21
#define EXECUTE_CACHE_9_10 23
#define EXECUTE_CACHE_9_7 25
#define FREE_REFERENCE_9_0 28
#define FREE_REFERENCES_LABEL_9_0 18
#define NUMBER_OF_LINKER_SECTIONS_9_1 2

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_9 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  machine_word Wrd26;
  machine_word Wrd18;
  machine_word Wrd22;
  machine_word Wrd21;
  machine_word Wrd7;
  machine_word Wrd12;
  machine_word Wrd11;
  machine_word Wrd8;
  machine_word Wrd15;
  machine_word Wrd20;
  machine_word Wrd19;
  machine_word Wrd16;
  machine_word Wrd14;
  machine_word Wrd5;
  machine_word Wrd10;
  machine_word Wrd9;
  machine_word Wrd6;
  machine_word Wrd29;
  machine_word Wrd28;
  machine_word Wrd27;
  INVOKE_INTERFACE_DECLS
  INVOKE_PRIMITIVE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_9_5);
      goto continuation_0;

    case 1:
      current_block = (Rpc - LABEL_9_4);
      goto fix_mit_source_dir_6;

    case 2:
      current_block = (Rpc - LABEL_9_8);
      goto label_8;

    case 3:
      current_block = (Rpc - LABEL_9_9);
      goto label_9;

    case 4:
      current_block = (Rpc - LABEL_9_6);
      goto continuation_1;

    case 5:
      current_block = (Rpc - LABEL_9_11);
      goto label_10;

    case 6:
      current_block = (Rpc - LABEL_9_13);
      goto label_11;

    case 7:
      current_block = (Rpc - LABEL_9_12);
      goto continuation_4;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (fix_mit_source_dir_13)
DEFLABEL (fix_mit_source_dir_6)
  INTERRUPT_CHECK (26, LABEL_9_4);
  (Wrd6.pObj) = ((SCHEME_OBJECT *) (current_block [FREE_REFERENCE_9_0]));
  (Wrd9.Obj) = ((Wrd6.pObj) [0]);
  (Wrd10.uLng) = (OBJECT_TYPE (Wrd9.Obj));
  if ((Wrd10.uLng) == 50)
    goto label_24;
  Wrd5 = Wrd9;

DEFLABEL (label_23)
  if (! ((Wrd5.Obj) == ((SCHEME_OBJECT) 0)))
    goto label_15;

DEFLABEL (label_14)
  Rvl = (Rsp [0]);
  Rsp = (& (Rsp [1]));
  goto pop_return;

DEFLABEL (label_15)
  (Wrd14.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_9_5]))));
  (* (--Rsp)) = (Wrd14.Obj);
  (Wrd16.pObj) = ((SCHEME_OBJECT *) (current_block [FREE_REFERENCE_9_0]));
  (Wrd19.Obj) = ((Wrd16.pObj) [0]);
  (Wrd20.uLng) = (OBJECT_TYPE (Wrd19.Obj));
  if ((Wrd20.uLng) == 50)
    goto label_22;
  Wrd15 = Wrd19;

DEFLABEL (label_21)
  (* (--Rsp)) = (Wrd15.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_9_10]));

DEFLABEL (continuation_0)
  INTERRUPT_CHECK (27, LABEL_9_5);
  if (! (Rvl == ((SCHEME_OBJECT) 0)))
    goto label_14;
  (Wrd27.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_9_6]))));
  (* (--Rsp)) = (Wrd27.Obj);
  (Wrd28.Obj) = (Rsp [1]);
  (* (--Rsp)) = (Wrd28.Obj);
  (Wrd29.Obj) = (current_block [OBJECT_9_0]);
  (* (--Rsp)) = (Wrd29.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_9_7]));

DEFLABEL (continuation_1)
  INTERRUPT_CHECK (27, LABEL_9_6);
  if (! (Rvl == ((SCHEME_OBJECT) 0)))
    goto label_16;
  Rvl = (Rsp [0]);
  Rsp = (& (Rsp [1]));
  goto pop_return;

DEFLABEL (label_16)
  (Wrd8.pObj) = ((SCHEME_OBJECT *) (current_block [FREE_REFERENCE_9_0]));
  (Wrd11.Obj) = ((Wrd8.pObj) [0]);
  (Wrd12.uLng) = (OBJECT_TYPE (Wrd11.Obj));
  if ((Wrd12.uLng) == 50)
    goto label_20;
  Wrd7 = Wrd11;

DEFLABEL (label_19)
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd15.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_9_12]))));
  (* (--Rsp)) = (Wrd15.Obj);
  (Wrd21.Obj) = (Rsp [2]);
  (Wrd22.uLng) = (OBJECT_TYPE (Wrd21.Obj));
  if (! ((Wrd22.uLng) == 30))
    goto label_18;
  (Wrd18.pObj) = (OBJECT_ADDRESS (Wrd21.Obj));
  (Wrd19.Obj) = ((Wrd18.pObj) [1]);
  (Wrd20.Obj) = (MAKE_OBJECT (26, (Wrd19.uLng)));
  (* (--Rsp)) = (Wrd20.Obj);

DEFLABEL (label_17)
  (Wrd27.Obj) = (current_block [OBJECT_9_2]);
  (* (--Rsp)) = (Wrd27.Obj);
  (Wrd28.Obj) = (Rsp [4]);
  (* (--Rsp)) = (Wrd28.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_9_14]));

DEFLABEL (continuation_4)
  INTERRUPT_CHECK (27, LABEL_9_12);
  (Rsp [1]) = Rvl;
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_9_15]));

DEFLABEL (label_18)
  (Wrd26.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_9_13]))));
  (* (--Rsp)) = (Wrd26.Obj);
  (* (--Rsp)) = (Wrd21.Obj);
  INVOKE_PRIMITIVE ((current_block [OBJECT_9_1]), 1);

DEFLABEL (label_11)
  (* (--Rsp)) = Rvl;
  goto label_17;

DEFLABEL (label_20)
  INVOKE_INTERFACE_2 (31, (& (current_block [LABEL_9_11])), (Wrd8.pObj));

DEFLABEL (label_10)
  (Wrd7.Obj) = Rvl;
  goto label_19;

DEFLABEL (label_22)
  INVOKE_INTERFACE_2 (31, (& (current_block [LABEL_9_9])), (Wrd16.pObj));

DEFLABEL (label_9)
  (Wrd15.Obj) = Rvl;
  goto label_21;

DEFLABEL (label_24)
  INVOKE_INTERFACE_2 (31, (& (current_block [LABEL_9_8])), (Wrd6.pObj));

DEFLABEL (label_8)
  (Wrd5.Obj) = Rvl;
  goto label_23;

INVOKE_INTERFACE_TARGET_1
INVOKE_PRIMITIVE_TARGET
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_10_4 3
#define LABEL_10_5 5
#define LABEL_10_7 7
#define LABEL_10_8 9
#define LABEL_10_10 11
#define LABEL_10_11 13
#define TAG_10_12 5
#define LABEL_10_15 15
#define LABEL_10_18 17
#define ENVIRONMENT_LABEL_10_3 41
#define DEBUGGING_LABEL_10_2 40
#define OBJECT_10_4 39
#define OBJECT_10_3 38
#define OBJECT_10_2 37
#define OBJECT_10_1 36
#define OBJECT_10_0 35
#define EXECUTE_CACHE_10_20 19
#define EXECUTE_CACHE_10_19 21
#define EXECUTE_CACHE_10_17 23
#define EXECUTE_CACHE_10_16 25
#define EXECUTE_CACHE_10_14 27
#define EXECUTE_CACHE_10_13 29
#define EXECUTE_CACHE_10_9 31
#define EXECUTE_CACHE_10_6 33
#define FREE_REFERENCES_LABEL_10_0 18
#define NUMBER_OF_LINKER_SECTIONS_10_1 1

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_10 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  machine_word Wrd17;
  machine_word Wrd16;
  machine_word Wrd6;
  machine_word Wrd5;
  machine_word Wrd14;
  machine_word Wrd13;
  machine_word Wrd8;
  machine_word Wrd9;
  machine_word Wrd15;
  machine_word Wrd12;
  machine_word Wrd11;
  machine_word Wrd10;
  machine_word Wrd7;
  INVOKE_INTERFACE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_10_4);
      goto geiser_eval_16;

    case 1:
      current_block = (Rpc - LABEL_10_5);
      goto continuation_0;

    case 2:
      current_block = (Rpc - LABEL_10_7);
      goto continuation_3;

    case 3:
      current_block = (Rpc - LABEL_10_8);
      goto continuation_2;

    case 4:
      current_block = (Rpc - LABEL_10_10);
      goto continuation_5;

    case 5:
      current_block = (Rpc - LABEL_10_11);
      goto lambda_20;

    case 6:
      current_block = (Rpc - LABEL_10_15);
      goto continuation_9;

    case 7:
      current_block = (Rpc - LABEL_10_18);
      goto continuation_6;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (geiser_eval_19)
DEFLABEL (geiser_eval_16)
  INTERRUPT_CHECK (26, LABEL_10_4);
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_10_5]))));
  (* (--Rsp)) = (Wrd7.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_10_6]));

DEFLABEL (continuation_0)
  INTERRUPT_CHECK (27, LABEL_10_5);
  (* (--Rsp)) = Rvl;
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_10_7]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd10.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_10_8]))));
  (* (--Rsp)) = (Wrd10.Obj);
  (Wrd11.Obj) = (current_block [OBJECT_10_0]);
  (* (--Rsp)) = (Wrd11.Obj);
  (Wrd12.Obj) = (Rsp [4]);
  if ((Wrd12.Obj) == ((SCHEME_OBJECT) 0))
    goto label_22;
  (* (--Rsp)) = (Wrd12.Obj);
  goto label_21;

DEFLABEL (label_22)
  (Wrd15.Obj) = (current_block [OBJECT_10_1]);
  (* (--Rsp)) = (Wrd15.Obj);

DEFLABEL (label_21)
DEFLABEL (label_23)
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_10_9]));

DEFLABEL (continuation_2)
  INTERRUPT_CHECK (27, LABEL_10_8);
  (* (--Rsp)) = Rvl;
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_10_14]));

DEFLABEL (continuation_3)
  INTERRUPT_CHECK (27, LABEL_10_7);
  (* (--Rsp)) = Rvl;
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_10_10]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (* (Rhp++)) = (MAKE_OBJECT (TC_MANIFEST_CLOSURE, 5));
  (Wrd9.pObj) = (Rhp + 1);
  Rhp += 1;
  WRITE_LABEL_DESCRIPTOR (Rhp, 0x101, 2);
  (* (Rhp++)) = (dispatch_base + TAG_10_12);
  (* (Rhp++)) = ((SCHEME_OBJECT) (& (current_block [LABEL_10_11])));
  Rhp += 2;
  (Wrd8.Obj) = (MAKE_POINTER_OBJECT (40, (Wrd9.pObj)));
  Wrd12 = Wrd9;
  (Wrd13.Obj) = (Rsp [4]);
  ((Wrd12.pObj) [2]) = (Wrd13.Obj);
  ((Wrd12.pObj) [3]) = Rvl;
  (* (--Rsp)) = (Wrd8.Obj);
  (Wrd14.Obj) = (Rsp [3]);
  (* (--Rsp)) = (Wrd14.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_10_13]));

DEFLABEL (continuation_5)
  INTERRUPT_CHECK (27, LABEL_10_10);
  (Rsp [0]) = Rvl;
  (Wrd8.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_10_15]))));
  (* (--Rsp)) = (Wrd8.Obj);
  (Wrd9.Obj) = (Rsp [2]);
  (* (--Rsp)) = (Wrd9.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_10_16]));

DEFLABEL (continuation_9)
  INTERRUPT_CHECK (27, LABEL_10_15);
  (Wrd9.Obj) = (current_block [OBJECT_10_2]);
  (* (Rhp++)) = (Wrd9.Obj);
  (* (Rhp++)) = Rvl;
  (Wrd8.pObj) = (& (Rhp [-2]));
  (Wrd6.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd8.pObj)));
  (Wrd13.Obj) = (current_block [OBJECT_10_3]);
  (* (Rhp++)) = (Wrd6.Obj);
  (* (Rhp++)) = (Wrd13.Obj);
  (Wrd11.pObj) = (& (Rhp [-2]));
  (Wrd12.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd11.pObj)));
  (* (--Rsp)) = (Wrd12.Obj);
  (Wrd16.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_10_18]))));
  (* (--Rsp)) = (Wrd16.Obj);
  (Wrd17.Obj) = (Rsp [2]);
  (* (--Rsp)) = (Wrd17.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_10_19]));

DEFLABEL (continuation_6)
  INTERRUPT_CHECK (27, LABEL_10_18);
  (Wrd9.Obj) = (current_block [OBJECT_10_3]);
  (* (Rhp++)) = Rvl;
  (* (Rhp++)) = (Wrd9.Obj);
  (Wrd8.pObj) = (& (Rhp [-2]));
  (Wrd6.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd8.pObj)));
  (Wrd13.Obj) = (current_block [OBJECT_10_4]);
  (* (Rhp++)) = (Wrd13.Obj);
  (* (Rhp++)) = (Wrd6.Obj);
  (Wrd12.pObj) = (& (Rhp [-2]));
  (Wrd10.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd12.pObj)));
  (Wrd14.Obj) = (* (Rsp++));
  (* (Rhp++)) = (Wrd10.Obj);
  (* (Rhp++)) = (Wrd14.Obj);
  (Wrd17.pObj) = (& (Rhp [-2]));
  (Wrd15.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd17.pObj)));
  (Rsp [4]) = (Wrd15.Obj);
  Rsp = (& (Rsp [4]));
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_10_20]));

DEFLABEL (lambda_20)
  CLOSURE_HEADER (LABEL_10_11);

DEFLABEL (lambda_4)
  CLOSURE_INTERRUPT_CHECK (24);
  (Wrd5.Obj) = (Rsp [0]);
  (Wrd6.pObj) = (OBJECT_ADDRESS (Wrd5.Obj));
  (Wrd7.Obj) = ((Wrd6.pObj) [2]);
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd8.Obj) = ((Wrd6.pObj) [3]);
  (Rsp [1]) = (Wrd8.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_10_17]));

INVOKE_INTERFACE_TARGET_0
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_11_4 3
#define LABEL_11_7 5
#define LABEL_11_5 7
#define LABEL_11_8 9
#define LABEL_11_9 11
#define LABEL_11_10 13
#define LABEL_11_12 15
#define LABEL_11_13 17
#define LABEL_11_15 19
#define ENVIRONMENT_LABEL_11_3 41
#define DEBUGGING_LABEL_11_2 40
#define OBJECT_11_8 39
#define OBJECT_11_7 38
#define OBJECT_11_6 37
#define OBJECT_11_5 36
#define OBJECT_11_4 35
#define OBJECT_11_3 34
#define OBJECT_11_2 33
#define OBJECT_11_1 32
#define OBJECT_11_0 31
#define EXECUTE_CACHE_11_17 21
#define EXECUTE_CACHE_11_16 23
#define EXECUTE_CACHE_11_14 25
#define EXECUTE_CACHE_11_11 27
#define EXECUTE_CACHE_11_6 29
#define FREE_REFERENCES_LABEL_11_0 20
#define NUMBER_OF_LINKER_SECTIONS_11_1 1

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_11 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  machine_word Wrd8;
  machine_word Wrd50;
  machine_word Wrd48;
  machine_word Wrd47;
  machine_word Wrd43;
  machine_word Wrd35;
  machine_word Wrd37;
  machine_word Wrd39;
  machine_word Wrd38;
  machine_word Wrd25;
  machine_word Wrd29;
  machine_word Wrd28;
  machine_word Wrd27;
  machine_word Wrd33;
  machine_word Wrd23;
  machine_word Wrd60;
  machine_word Wrd57;
  machine_word Wrd22;
  machine_word Wrd21;
  machine_word Wrd20;
  machine_word Wrd55;
  machine_word Wrd52;
  machine_word Wrd18;
  machine_word Wrd12;
  machine_word Wrd11;
  machine_word Wrd14;
  machine_word Wrd13;
  machine_word Wrd62;
  machine_word Wrd64;
  machine_word Wrd66;
  machine_word Wrd65;
  machine_word Wrd9;
  machine_word Wrd6;
  machine_word Wrd5;
  INVOKE_INTERFACE_DECLS
  INVOKE_PRIMITIVE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_11_4);
      goto geiser_autodoc_12;

    case 1:
      current_block = (Rpc - LABEL_11_7);
      goto continuation_3;

    case 2:
      current_block = (Rpc - LABEL_11_5);
      goto continuation_1;

    case 3:
      current_block = (Rpc - LABEL_11_8);
      goto label_14;

    case 4:
      current_block = (Rpc - LABEL_11_9);
      goto label_16;

    case 5:
      current_block = (Rpc - LABEL_11_10);
      goto label_15;

    case 6:
      current_block = (Rpc - LABEL_11_12);
      goto continuation_8;

    case 7:
      current_block = (Rpc - LABEL_11_13);
      goto lambda_7;

    case 8:
      current_block = (Rpc - LABEL_11_15);
      goto continuation_6;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (geiser_autodoc_18)
DEFLABEL (geiser_autodoc_12)
  INTERRUPT_CHECK (26, LABEL_11_4);
  (Wrd5.Obj) = (Rsp [0]);
  (Wrd6.Obj) = (current_block [OBJECT_11_0]);
  if (! ((Wrd5.Obj) == (Wrd6.Obj)))
    goto label_20;
  Rvl = (Wrd6.Obj);
  Rsp = (& (Rsp [2]));
  goto pop_return;

DEFLABEL (label_20)
  (Wrd9.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_11_5]))));
  (* (--Rsp)) = (Wrd9.Obj);
  (* (--Rsp)) = (Wrd5.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_11_6]));

DEFLABEL (continuation_1)
  INTERRUPT_CHECK (27, LABEL_11_5);
  if (Rvl == ((SCHEME_OBJECT) 0))
    goto label_35;
  (Wrd9.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_11_7]))));
  (* (--Rsp)) = (Wrd9.Obj);
  (Wrd13.Obj) = (Rsp [1]);
  (Wrd14.uLng) = (OBJECT_TYPE (Wrd13.Obj));
  if (! ((Wrd14.uLng) == 1))
    goto label_34;
  (Wrd11.pObj) = (OBJECT_ADDRESS (Wrd13.Obj));
  (Wrd12.Obj) = ((Wrd11.pObj) [0]);
  (* (--Rsp)) = (Wrd12.Obj);

DEFLABEL (label_33)
  (Wrd52.Obj) = (current_block [OBJECT_11_2]);
  (Wrd55.Lng) = (1 << TYPE_CODE_LENGTH);
  if (! (((unsigned long) 29L) < ((unsigned long) (Wrd55.Lng))))
    goto label_32;
  (Wrd20.uLng) = (OBJECT_DATUM (Wrd52.Obj));
  (Wrd21.Obj) = (Rsp [0]);
  (Wrd22.uLng) = (OBJECT_TYPE (Wrd21.Obj));
  if ((Wrd20.Obj) == (Wrd22.Obj))
    goto label_31;

DEFLABEL (label_30)
  (Wrd23.Obj) = (current_block [OBJECT_11_4]);
  (* (--Rsp)) = (Wrd23.Obj);
  (Wrd33.Lng) = (1 << TYPE_CODE_LENGTH);
  if (((unsigned long) 5L) < ((unsigned long) (Wrd33.Lng)))
    goto label_21;
  INVOKE_PRIMITIVE ((current_block [OBJECT_11_3]), 2);

DEFLABEL (label_21)
  (Wrd27.uLng) = (OBJECT_DATUM (Wrd23.Obj));
  (Wrd28.Obj) = (Rsp [1]);
  (Wrd29.uLng) = (OBJECT_TYPE (Wrd28.Obj));
  if ((Wrd27.Obj) == (Wrd29.Obj))
    goto label_23;
  (Wrd25.Obj) = (current_block [OBJECT_11_6]);
  goto label_22;

DEFLABEL (label_23)
  (Wrd25.Obj) = (current_block [OBJECT_11_5]);

DEFLABEL (label_22)
DEFLABEL (label_29)
  Rsp = (& (Rsp [3]));
  if ((Wrd25.Obj) == ((SCHEME_OBJECT) 0))
    goto label_25;

DEFLABEL (label_24)
  (Wrd47.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_11_12]))));
  (* (--Rsp)) = (Wrd47.Obj);
  (Wrd48.Obj) = (Rsp [1]);
  (* (--Rsp)) = (Wrd48.Obj);
  (Wrd50.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_11_13]))));
  (* (--Rsp)) = (Wrd50.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_11_14]));

DEFLABEL (continuation_8)
  INTERRUPT_CHECK (27, LABEL_11_12);
  (Rsp [1]) = Rvl;
  Rsp = (& (Rsp [1]));
  Rvl = (Rsp [0]);
  Rsp = (& (Rsp [1]));
  goto pop_return;

DEFLABEL (label_25)
  (Wrd38.Obj) = (Rsp [0]);
  (Wrd39.uLng) = (OBJECT_TYPE (Wrd38.Obj));
  if (! ((Wrd39.uLng) == 1))
    goto label_28;
  (Wrd37.pObj) = (OBJECT_ADDRESS (Wrd38.Obj));
  (Wrd35.Obj) = ((Wrd37.pObj) [1]);

DEFLABEL (label_27)
  (Rsp [1]) = (Wrd35.Obj);

DEFLABEL (label_26)
  Rsp = (& (Rsp [1]));
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_11_11]));

DEFLABEL (label_28)
  (Wrd43.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_11_10]))));
  (* (--Rsp)) = (Wrd43.Obj);
  (* (--Rsp)) = (Wrd38.Obj);
  INVOKE_PRIMITIVE ((current_block [OBJECT_11_7]), 1);

DEFLABEL (label_15)
  (Wrd35.Obj) = Rvl;
  goto label_27;

DEFLABEL (label_31)
  Rsp = (& (Rsp [2]));
  goto label_24;

DEFLABEL (label_32)
  (Wrd57.Obj) = (Rsp [0]);
  (Wrd60.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_11_9]))));
  (* (--Rsp)) = (Wrd60.Obj);
  (* (--Rsp)) = (Wrd57.Obj);
  (* (--Rsp)) = (Wrd52.Obj);
  INVOKE_PRIMITIVE ((current_block [OBJECT_11_3]), 2);

DEFLABEL (label_16)
  if (Rvl == ((SCHEME_OBJECT) 0))
    goto label_30;
  goto label_31;

DEFLABEL (label_34)
  (Wrd18.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_11_8]))));
  (* (--Rsp)) = (Wrd18.Obj);
  (* (--Rsp)) = (Wrd13.Obj);
  INVOKE_PRIMITIVE ((current_block [OBJECT_11_1]), 1);

DEFLABEL (label_14)
  (* (--Rsp)) = Rvl;
  goto label_33;

DEFLABEL (label_35)
  (Wrd65.Obj) = (Rsp [0]);
  (Wrd66.Obj) = (current_block [OBJECT_11_0]);
  (* (Rhp++)) = (Wrd65.Obj);
  (* (Rhp++)) = (Wrd66.Obj);
  (Wrd64.pObj) = (& (Rhp [-2]));
  (Wrd62.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd64.pObj)));
  (Rsp [1]) = (Wrd62.Obj);
  goto label_26;

DEFLABEL (continuation_3)
  INTERRUPT_CHECK (27, LABEL_11_7);
  if (Rvl == ((SCHEME_OBJECT) 0))
    goto label_25;
  goto label_24;

DEFLABEL (lambda_19)
DEFLABEL (lambda_7)
  INTERRUPT_CHECK (26, LABEL_11_13);
  (Wrd5.Obj) = (Rsp [0]);
  (* (--Rsp)) = (Wrd5.Obj);
  (Wrd8.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_11_15]))));
  (* (--Rsp)) = (Wrd8.Obj);
  (Wrd9.Obj) = (current_block [OBJECT_11_8]);
  (* (--Rsp)) = (Wrd9.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_11_16]));

DEFLABEL (continuation_6)
  INTERRUPT_CHECK (27, LABEL_11_15);
  (Rsp [1]) = Rvl;
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_11_17]));

INVOKE_INTERFACE_TARGET_1
INVOKE_PRIMITIVE_TARGET
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_12_4 3
#define LABEL_12_5 5
#define TAG_12_6 1
#define LABEL_12_7 7
#define LABEL_12_8 9
#define LABEL_12_10 11
#define LABEL_12_13 13
#define LABEL_12_16 15
#define ENVIRONMENT_LABEL_12_3 32
#define DEBUGGING_LABEL_12_2 31
#define EXECUTE_CACHE_12_18 17
#define EXECUTE_CACHE_12_17 19
#define EXECUTE_CACHE_12_15 21
#define EXECUTE_CACHE_12_14 23
#define EXECUTE_CACHE_12_12 25
#define EXECUTE_CACHE_12_11 27
#define EXECUTE_CACHE_12_9 29
#define FREE_REFERENCES_LABEL_12_0 16
#define NUMBER_OF_LINKER_SECTIONS_12_1 1

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_12 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  machine_word Wrd15;
  machine_word Wrd12;
  machine_word Wrd9;
  machine_word Wrd8;
  machine_word Wrd6;
  machine_word Wrd7;
  INVOKE_INTERFACE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_12_4);
      goto geiser_module_completions_6;

    case 1:
      current_block = (Rpc - LABEL_12_5);
      goto lambda_10;

    case 2:
      current_block = (Rpc - LABEL_12_7);
      goto continuation_5;

    case 3:
      current_block = (Rpc - LABEL_12_8);
      goto continuation_4;

    case 4:
      current_block = (Rpc - LABEL_12_10);
      goto continuation_0;

    case 5:
      current_block = (Rpc - LABEL_12_13);
      goto lambda_3;

    case 6:
      current_block = (Rpc - LABEL_12_16);
      goto continuation_2;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (geiser_module_completions_9)
DEFLABEL (geiser_module_completions_6)
  INTERRUPT_CHECK (26, LABEL_12_4);
  (* (Rhp++)) = (MAKE_OBJECT (TC_MANIFEST_CLOSURE, 4));
  (Wrd7.pObj) = (Rhp + 1);
  Rhp += 1;
  WRITE_LABEL_DESCRIPTOR (Rhp, 0x202, 2);
  (* (Rhp++)) = (dispatch_base + TAG_12_6);
  (* (Rhp++)) = ((SCHEME_OBJECT) (& (current_block [LABEL_12_5])));
  Rhp += 1;
  (Wrd6.Obj) = (MAKE_POINTER_OBJECT (40, (Wrd7.pObj)));
  Wrd8 = Wrd7;
  (Wrd9.Obj) = (Rsp [0]);
  ((Wrd8.pObj) [2]) = (Wrd9.Obj);
  (Rsp [0]) = (Wrd6.Obj);
  (Wrd12.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_12_7]))));
  (* (--Rsp)) = (Wrd12.Obj);
  (Wrd15.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_12_8]))));
  (* (--Rsp)) = (Wrd15.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_12_9]));

DEFLABEL (continuation_4)
  INTERRUPT_CHECK (27, LABEL_12_8);
  (* (--Rsp)) = Rvl;
  (Wrd6.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_12_13]))));
  (* (--Rsp)) = (Wrd6.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_12_14]));

DEFLABEL (continuation_5)
  INTERRUPT_CHECK (27, LABEL_12_7);
  (Rsp [1]) = Rvl;
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_12_12]));

DEFLABEL (lambda_10)
  CLOSURE_HEADER (LABEL_12_5);

DEFLABEL (lambda_1)
  CLOSURE_INTERRUPT_CHECK (24);
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_12_10]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd8.Obj) = (Rsp [2]);
  (* (--Rsp)) = (Wrd8.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_12_11]));

DEFLABEL (continuation_0)
  INTERRUPT_CHECK (27, LABEL_12_10);
  (Rsp [1]) = Rvl;
  (Wrd7.Obj) = (Rsp [0]);
  (Wrd8.pObj) = (OBJECT_ADDRESS (Wrd7.Obj));
  (Wrd6.Obj) = ((Wrd8.pObj) [2]);
  (Rsp [0]) = (Wrd6.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_12_15]));

DEFLABEL (lambda_11)
DEFLABEL (lambda_3)
  INTERRUPT_CHECK (26, LABEL_12_13);
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_12_16]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd8.Obj) = (Rsp [1]);
  (* (--Rsp)) = (Wrd8.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_12_17]));

DEFLABEL (continuation_2)
  INTERRUPT_CHECK (27, LABEL_12_16);
  (Rsp [0]) = Rvl;
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_12_18]));

INVOKE_INTERFACE_TARGET_0
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_13_4 3
#define LABEL_13_5 5
#define LABEL_13_8 7
#define LABEL_13_6 9
#define ENVIRONMENT_LABEL_13_3 21
#define DEBUGGING_LABEL_13_2 20
#define OBJECT_13_0 19
#define EXECUTE_CACHE_13_10 11
#define EXECUTE_CACHE_13_9 13
#define EXECUTE_CACHE_13_7 15
#define FREE_REFERENCE_13_0 18
#define FREE_REFERENCES_LABEL_13_0 10
#define NUMBER_OF_LINKER_SECTIONS_13_1 2

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_13 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  machine_word Wrd5;
  machine_word Wrd12;
  machine_word Wrd8;
  machine_word Wrd11;
  machine_word Wrd10;
  machine_word Wrd7;
  INVOKE_INTERFACE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_13_4);
      goto geiser_completions_2;

    case 1:
      current_block = (Rpc - LABEL_13_5);
      goto continuation_1;

    case 2:
      current_block = (Rpc - LABEL_13_8);
      goto label_4;

    case 3:
      current_block = (Rpc - LABEL_13_6);
      goto continuation_0;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (geiser_completions_6)
DEFLABEL (geiser_completions_2)
  INTERRUPT_CHECK (26, LABEL_13_4);
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_13_5]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd10.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_13_6]))));
  (* (--Rsp)) = (Wrd10.Obj);
  (Wrd11.Obj) = (current_block [OBJECT_13_0]);
  (* (--Rsp)) = (Wrd11.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_13_7]));

DEFLABEL (continuation_0)
  INTERRUPT_CHECK (27, LABEL_13_6);
  (* (--Rsp)) = Rvl;
  (Wrd5.Obj) = (Rsp [2]);
  (* (--Rsp)) = (Wrd5.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_13_10]));

DEFLABEL (continuation_1)
  INTERRUPT_CHECK (27, LABEL_13_5);
  (Rsp [0]) = Rvl;
  (Wrd8.pObj) = ((SCHEME_OBJECT *) (current_block [FREE_REFERENCE_13_0]));
  (Wrd11.Obj) = ((Wrd8.pObj) [0]);
  (Wrd12.uLng) = (OBJECT_TYPE (Wrd11.Obj));
  if ((Wrd12.uLng) == 50)
    goto label_8;
  Wrd7 = Wrd11;

DEFLABEL (label_7)
  (Rsp [1]) = (Wrd7.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_13_9]));

DEFLABEL (label_8)
  INVOKE_INTERFACE_2 (31, (& (current_block [LABEL_13_8])), (Wrd8.pObj));

DEFLABEL (label_4)
  (Wrd7.Obj) = Rvl;
  goto label_7;

INVOKE_INTERFACE_TARGET_1
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_14_4 3
#define LABEL_14_5 5
#define LABEL_14_6 7
#define LABEL_14_8 9
#define LABEL_14_11 11
#define ENVIRONMENT_LABEL_14_3 25
#define DEBUGGING_LABEL_14_2 24
#define OBJECT_14_0 23
#define EXECUTE_CACHE_14_12 13
#define EXECUTE_CACHE_14_10 15
#define EXECUTE_CACHE_14_9 17
#define EXECUTE_CACHE_14_7 19
#define FREE_ASSIGNMENT_14_0 22
#define FREE_REFERENCES_LABEL_14_0 12
#define NUMBER_OF_LINKER_SECTIONS_14_1 2

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_14 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  machine_word Wrd14;
  machine_word Wrd13;
  machine_word Wrd6;
  machine_word Wrd5;
  machine_word Wrd12;
  machine_word Wrd11;
  machine_word Wrd10;
  machine_word Wrd7;
  INVOKE_INTERFACE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_14_4);
      goto geiser_ge_5;

    case 1:
      current_block = (Rpc - LABEL_14_5);
      goto continuation_1;

    case 2:
      current_block = (Rpc - LABEL_14_6);
      goto continuation_0;

    case 3:
      current_block = (Rpc - LABEL_14_8);
      goto continuation_3;

    case 4:
      current_block = (Rpc - LABEL_14_11);
      goto label_7;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (geiser_ge_9)
DEFLABEL (geiser_ge_5)
  INTERRUPT_CHECK (26, LABEL_14_4);
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_14_5]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd10.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_14_6]))));
  (* (--Rsp)) = (Wrd10.Obj);
  (Wrd11.Obj) = (current_block [OBJECT_14_0]);
  (* (--Rsp)) = (Wrd11.Obj);
  (Wrd12.Obj) = (Rsp [3]);
  (* (--Rsp)) = (Wrd12.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_14_7]));

DEFLABEL (continuation_0)
  INTERRUPT_CHECK (27, LABEL_14_6);
  (* (--Rsp)) = Rvl;
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_14_10]));

DEFLABEL (continuation_1)
  INTERRUPT_CHECK (27, LABEL_14_5);
  (* (--Rsp)) = Rvl;
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_14_8]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (* (--Rsp)) = Rvl;
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_14_9]));

DEFLABEL (continuation_3)
  INTERRUPT_CHECK (27, LABEL_14_8);
  (Wrd5.pObj) = ((SCHEME_OBJECT *) (current_block [FREE_ASSIGNMENT_14_0]));
  (Wrd6.Obj) = (Rsp [0]);
  (Wrd13.Obj) = ((Wrd5.pObj) [0]);
  (Wrd14.uLng) = (OBJECT_TYPE (Wrd13.Obj));
  if ((Wrd14.uLng) == 50)
    goto label_12;

DEFLABEL (label_11)
  ((Wrd5.pObj) [0]) = (Wrd6.Obj);

DEFLABEL (label_10)
  Rsp = (& (Rsp [1]));
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_14_12]));

DEFLABEL (label_12)
  if ((Wrd13.Obj) == (MAKE_OBJECT (50, 0)))
    goto label_11;
  INVOKE_INTERFACE_3 (29, (& (current_block [LABEL_14_11])), (Wrd5.pObj), (Wrd6.Obj));

DEFLABEL (label_7)
  goto label_10;

INVOKE_INTERFACE_TARGET_1
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_15_4 3
#define ENVIRONMENT_LABEL_15_3 8
#define DEBUGGING_LABEL_15_2 7
#define EXECUTE_CACHE_15_5 5
#define FREE_REFERENCES_LABEL_15_0 4
#define NUMBER_OF_LINKER_SECTIONS_15_1 1

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_15 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  INVOKE_INTERFACE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_15_4);
      goto geiser_load_file_0;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (geiser_load_file_3)
DEFLABEL (geiser_load_file_0)
  INTERRUPT_CHECK (26, LABEL_15_4);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_15_5]));

INVOKE_INTERFACE_TARGET_1
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_16_4 3
#define LABEL_16_5 5
#define LABEL_16_7 7
#define LABEL_16_8 9
#define LABEL_16_12 11
#define LABEL_16_10 13
#define LABEL_16_14 15
#define LABEL_16_16 17
#define LABEL_16_17 19
#define LABEL_16_19 21
#define LABEL_16_20 23
#define LABEL_16_21 25
#define LABEL_16_22 27
#define LABEL_16_23 29
#define LABEL_16_24 31
#define LABEL_16_25 33
#define LABEL_16_27 35
#define LABEL_16_18 37
#define LABEL_16_28 39
#define LABEL_16_30 41
#define LABEL_16_31 43
#define LABEL_16_33 45
#define ENVIRONMENT_LABEL_16_3 79
#define DEBUGGING_LABEL_16_2 78
#define OBJECT_16_11 77
#define OBJECT_16_10 76
#define OBJECT_16_9 75
#define OBJECT_16_8 74
#define OBJECT_16_7 73
#define OBJECT_16_6 72
#define OBJECT_16_5 71
#define OBJECT_16_4 70
#define OBJECT_16_3 69
#define OBJECT_16_2 68
#define OBJECT_16_1 67
#define OBJECT_16_0 66
#define EXECUTE_CACHE_16_32 47
#define EXECUTE_CACHE_16_29 49
#define EXECUTE_CACHE_16_26 51
#define EXECUTE_CACHE_16_15 53
#define EXECUTE_CACHE_16_13 55
#define EXECUTE_CACHE_16_11 57
#define EXECUTE_CACHE_16_9 59
#define EXECUTE_CACHE_16_6 61
#define FREE_REFERENCE_16_1 64
#define FREE_REFERENCE_16_0 65
#define FREE_REFERENCES_LABEL_16_0 46
#define NUMBER_OF_LINKER_SECTIONS_16_1 2

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_16 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  machine_word Wrd37;
  machine_word Wrd34;
  machine_word Wrd33;
  machine_word Wrd29;
  machine_word Wrd25;
  machine_word Wrd21;
  machine_word Wrd12;
  machine_word Wrd26;
  machine_word Wrd22;
  machine_word Wrd23;
  machine_word Wrd16;
  machine_word Wrd13;
  machine_word Wrd14;
  machine_word Wrd59;
  machine_word Wrd58;
  machine_word Wrd57;
  machine_word Wrd54;
  machine_word Wrd48;
  machine_word Wrd49;
  machine_word Wrd50;
  machine_word Wrd46;
  machine_word Wrd38;
  machine_word Wrd40;
  machine_word Wrd42;
  machine_word Wrd41;
  machine_word Wrd61;
  machine_word Wrd36;
  machine_word Wrd28;
  machine_word Wrd30;
  machine_word Wrd32;
  machine_word Wrd31;
  machine_word Wrd27;
  machine_word Wrd24;
  machine_word Wrd18;
  machine_word Wrd17;
  machine_word Wrd20;
  machine_word Wrd19;
  machine_word Wrd15;
  machine_word Wrd66;
  machine_word Wrd71;
  machine_word Wrd70;
  machine_word Wrd67;
  machine_word Wrd65;
  machine_word Wrd64;
  machine_word Wrd5;
  machine_word Wrd10;
  machine_word Wrd6;
  machine_word Wrd11;
  machine_word Wrd9;
  machine_word Wrd8;
  machine_word Wrd7;
  INVOKE_INTERFACE_DECLS
  INVOKE_PRIMITIVE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_16_4);
      goto geiser_module_exports_46;

    case 1:
      current_block = (Rpc - LABEL_16_5);
      goto continuation_0;

    case 2:
      current_block = (Rpc - LABEL_16_7);
      goto continuation_2;

    case 3:
      current_block = (Rpc - LABEL_16_8);
      goto continuation_1;

    case 4:
      current_block = (Rpc - LABEL_16_12);
      goto label_48;

    case 5:
      current_block = (Rpc - LABEL_16_10);
      goto continuation_3;

    case 6:
      current_block = (Rpc - LABEL_16_14);
      goto continuation_4;

    case 7:
      current_block = (Rpc - LABEL_16_16);
      goto continuation_11;

    case 8:
      current_block = (Rpc - LABEL_16_17);
      goto loop_41;

    case 9:
      current_block = (Rpc - LABEL_16_19);
      goto label_54;

    case 10:
      current_block = (Rpc - LABEL_16_20);
      goto label_49;

    case 11:
      current_block = (Rpc - LABEL_16_21);
      goto label_50;

    case 12:
      current_block = (Rpc - LABEL_16_22);
      goto label_51;

    case 13:
      current_block = (Rpc - LABEL_16_23);
      goto label_52;

    case 14:
      current_block = (Rpc - LABEL_16_24);
      goto label_53;

    case 15:
      current_block = (Rpc - LABEL_16_25);
      goto continuation_13;

    case 16:
      current_block = (Rpc - LABEL_16_27);
      goto label_55;

    case 17:
      current_block = (Rpc - LABEL_16_18);
      goto continuation_36;

    case 18:
      current_block = (Rpc - LABEL_16_28);
      goto continuation_15;

    case 19:
      current_block = (Rpc - LABEL_16_30);
      goto label_56;

    case 20:
      current_block = (Rpc - LABEL_16_31);
      goto continuation_19;

    case 21:
      current_block = (Rpc - LABEL_16_33);
      goto label_57;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (geiser_module_exports_59)
DEFLABEL (geiser_module_exports_46)
  INTERRUPT_CHECK (26, LABEL_16_4);
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_16_5]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd8.Obj) = (current_block [OBJECT_16_0]);
  (* (--Rsp)) = (Wrd8.Obj);
  (Wrd9.Obj) = (Rsp [2]);
  (* (--Rsp)) = (Wrd9.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_16_6]));

DEFLABEL (continuation_0)
  INTERRUPT_CHECK (27, LABEL_16_5);
  (Rsp [0]) = Rvl;
  (Wrd8.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_16_7]))));
  (* (--Rsp)) = (Wrd8.Obj);
  (Wrd11.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_16_8]))));
  (* (--Rsp)) = (Wrd11.Obj);
  (* (--Rsp)) = Rvl;
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_16_9]));

DEFLABEL (continuation_1)
  INTERRUPT_CHECK (27, LABEL_16_8);
  (* (--Rsp)) = Rvl;
  (Wrd6.pObj) = ((SCHEME_OBJECT *) (current_block [FREE_REFERENCE_16_0]));
  (Wrd9.Obj) = ((Wrd6.pObj) [0]);
  (Wrd10.uLng) = (OBJECT_TYPE (Wrd9.Obj));
  if ((Wrd10.uLng) == 50)
    goto label_62;
  Wrd5 = Wrd9;

DEFLABEL (label_61)
  (* (--Rsp)) = (Wrd5.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_16_13]));

DEFLABEL (continuation_2)
  INTERRUPT_CHECK (27, LABEL_16_7);
  (* (--Rsp)) = Rvl;
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_16_10]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd8.Obj) = (Rsp [2]);
  (* (--Rsp)) = (Wrd8.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_16_11]));

DEFLABEL (continuation_3)
  INTERRUPT_CHECK (27, LABEL_16_10);
  (* (--Rsp)) = Rvl;
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_16_14]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (* (--Rsp)) = Rvl;
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_16_15]));

DEFLABEL (continuation_4)
  INTERRUPT_CHECK (27, LABEL_16_14);
  (* (--Rsp)) = Rvl;
  (Wrd5.Obj) = (current_block [OBJECT_16_1]);
  (* (--Rsp)) = (Wrd5.Obj);
  (* (--Rsp)) = (Wrd5.Obj);
  (* (--Rsp)) = (Wrd5.Obj);
  goto loop_41;

DEFLABEL (label_62)
  INVOKE_INTERFACE_2 (31, (& (current_block [LABEL_16_12])), (Wrd6.pObj));

DEFLABEL (label_48)
  (Wrd5.Obj) = Rvl;
  goto label_61;

DEFLABEL (loop_60)
DEFLABEL (loop_41)
  INTERRUPT_CHECK (26, LABEL_16_17);
  (Wrd5.Obj) = (Rsp [3]);
  if ((Wrd5.Obj) == (current_block [OBJECT_16_1]))
    goto label_83;
  (Wrd11.uLng) = (OBJECT_TYPE (Wrd5.Obj));
  if (! ((Wrd11.uLng) == 1))
    goto label_82;
  (Wrd8.pObj) = (OBJECT_ADDRESS (Wrd5.Obj));
  (Wrd9.Obj) = ((Wrd8.pObj) [0]);
  (* (--Rsp)) = (Wrd9.Obj);

DEFLABEL (label_81)
  (Wrd19.Obj) = (Rsp [0]);
  (Wrd20.uLng) = (OBJECT_TYPE (Wrd19.Obj));
  if (! ((Wrd20.uLng) == 1))
    goto label_80;
  (Wrd17.pObj) = (OBJECT_ADDRESS (Wrd19.Obj));
  (Wrd18.Obj) = ((Wrd17.pObj) [0]);
  (* (--Rsp)) = (Wrd18.Obj);

DEFLABEL (label_79)
  (Wrd27.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_16_16]))));
  (* (--Rsp)) = (Wrd27.Obj);
  (Wrd31.Obj) = (Rsp [2]);
  (Wrd32.uLng) = (OBJECT_TYPE (Wrd31.Obj));
  if (! ((Wrd32.uLng) == 1))
    goto label_78;
  (Wrd30.pObj) = (OBJECT_ADDRESS (Wrd31.Obj));
  (Wrd28.Obj) = ((Wrd30.pObj) [1]);

DEFLABEL (label_77)
  if ((Wrd28.Obj) == (current_block [OBJECT_16_1]))
    goto label_76;
  (Wrd41.Obj) = (Rsp [2]);
  (Wrd42.uLng) = (OBJECT_TYPE (Wrd41.Obj));
  if (! ((Wrd42.uLng) == 1))
    goto label_75;
  (Wrd40.pObj) = (OBJECT_ADDRESS (Wrd41.Obj));
  (Wrd38.Obj) = ((Wrd40.pObj) [1]);

DEFLABEL (label_74)
  (Wrd50.uLng) = (OBJECT_TYPE (Wrd38.Obj));
  if (! ((Wrd50.uLng) == 1))
    goto label_73;
  (Wrd49.pObj) = (OBJECT_ADDRESS (Wrd38.Obj));
  (Wrd48.Obj) = ((Wrd49.pObj) [0]);

DEFLABEL (label_72)
  Rsp = (& (Rsp [1]));
  (* (--Rsp)) = (Wrd48.Obj);

DEFLABEL (label_71)
  (Wrd57.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_16_25]))));
  (* (--Rsp)) = (Wrd57.Obj);
  (Wrd58.Obj) = (Rsp [2]);
  (* (--Rsp)) = (Wrd58.Obj);
  (Wrd59.Obj) = (Rsp [9]);
  (* (--Rsp)) = (Wrd59.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_16_26]));

DEFLABEL (continuation_13)
  INTERRUPT_CHECK (27, LABEL_16_25);
  (* (--Rsp)) = Rvl;
  if (Rvl == (current_block [OBJECT_16_5]))
    goto label_68;
  (Wrd9.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_16_28]))));
  (* (--Rsp)) = (Wrd9.Obj);
  (Wrd10.Obj) = (Rsp [2]);
  (* (--Rsp)) = (Wrd10.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_16_29]));

DEFLABEL (continuation_15)
  INTERRUPT_CHECK (27, LABEL_16_28);
  if (Rvl == ((SCHEME_OBJECT) 0))
    goto label_65;
  (Wrd9.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_16_31]))));
  (* (--Rsp)) = (Wrd9.Obj);
  (Wrd10.Obj) = (Rsp [9]);
  (* (--Rsp)) = (Wrd10.Obj);
  (Wrd11.Obj) = (Rsp [4]);
  (* (--Rsp)) = (Wrd11.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_16_32]));

DEFLABEL (continuation_19)
  INTERRUPT_CHECK (27, LABEL_16_31);
  (Wrd9.Obj) = (current_block [OBJECT_16_11]);
  (* (Rhp++)) = (Wrd9.Obj);
  (* (Rhp++)) = Rvl;
  (Wrd8.pObj) = (& (Rhp [-2]));
  (Wrd6.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd8.pObj)));
  (Wrd13.Obj) = (current_block [OBJECT_16_1]);
  (* (Rhp++)) = (Wrd6.Obj);
  (* (Rhp++)) = (Wrd13.Obj);
  (Wrd12.pObj) = (& (Rhp [-2]));
  (Wrd10.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd12.pObj)));
  (Wrd17.Obj) = (Rsp [2]);
  (* (Rhp++)) = (Wrd17.Obj);
  (* (Rhp++)) = (Wrd10.Obj);
  (Wrd16.pObj) = (& (Rhp [-2]));
  (Wrd14.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd16.pObj)));
  (Wrd21.Obj) = (Rsp [5]);
  (* (Rhp++)) = (Wrd14.Obj);
  (* (Rhp++)) = (Wrd21.Obj);
  (Wrd20.pObj) = (& (Rhp [-2]));
  (Wrd18.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd20.pObj)));
  (Rsp [5]) = (Wrd18.Obj);
  (Wrd25.Obj) = (Rsp [7]);
  (Wrd26.uLng) = (OBJECT_TYPE (Wrd25.Obj));
  if (! ((Wrd26.uLng) == 1))
    goto label_64;
  (Wrd24.pObj) = (OBJECT_ADDRESS (Wrd25.Obj));
  (Wrd22.Obj) = ((Wrd24.pObj) [1]);

DEFLABEL (label_63)
  (Rsp [7]) = (Wrd22.Obj);
  Rsp = (& (Rsp [4]));
  goto loop_41;

DEFLABEL (label_64)
  (Wrd30.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_16_33]))));
  (* (--Rsp)) = (Wrd30.Obj);
  (* (--Rsp)) = (Wrd25.Obj);
  INVOKE_PRIMITIVE ((current_block [OBJECT_16_3]), 1);

DEFLABEL (label_57)
  (Wrd22.Obj) = Rvl;
  goto label_63;

DEFLABEL (label_65)
  (Wrd15.Obj) = (Rsp [2]);
  (Wrd16.Obj) = (current_block [OBJECT_16_1]);
  (* (Rhp++)) = (Wrd15.Obj);
  (* (Rhp++)) = (Wrd16.Obj);
  (Wrd14.pObj) = (& (Rhp [-2]));
  (Wrd12.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd14.pObj)));
  (Wrd20.Obj) = (Rsp [4]);
  (* (Rhp++)) = (Wrd12.Obj);
  (* (Rhp++)) = (Wrd20.Obj);
  (Wrd19.pObj) = (& (Rhp [-2]));
  (Wrd17.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd19.pObj)));
  (Rsp [4]) = (Wrd17.Obj);
  (Wrd24.Obj) = (Rsp [7]);
  (Wrd25.uLng) = (OBJECT_TYPE (Wrd24.Obj));
  if (! ((Wrd25.uLng) == 1))
    goto label_67;
  (Wrd23.pObj) = (OBJECT_ADDRESS (Wrd24.Obj));
  (Wrd21.Obj) = ((Wrd23.pObj) [1]);

DEFLABEL (label_66)
  (Rsp [7]) = (Wrd21.Obj);
  Rsp = (& (Rsp [4]));
  goto loop_41;

DEFLABEL (label_67)
  (Wrd29.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_16_30]))));
  (* (--Rsp)) = (Wrd29.Obj);
  (* (--Rsp)) = (Wrd24.Obj);
  INVOKE_PRIMITIVE ((current_block [OBJECT_16_3]), 1);

DEFLABEL (label_56)
  (Wrd21.Obj) = Rvl;
  goto label_66;

DEFLABEL (label_68)
  (Wrd14.Obj) = (current_block [OBJECT_16_6]);
  (Wrd15.Obj) = (current_block [OBJECT_16_1]);
  (* (Rhp++)) = (Wrd14.Obj);
  (* (Rhp++)) = (Wrd15.Obj);
  (Wrd13.pObj) = (& (Rhp [-2]));
  (Wrd11.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd13.pObj)));
  (Wrd19.Obj) = (Rsp [2]);
  (* (Rhp++)) = (Wrd19.Obj);
  (* (Rhp++)) = (Wrd11.Obj);
  (Wrd18.pObj) = (& (Rhp [-2]));
  (Wrd16.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd18.pObj)));
  (Wrd23.Obj) = (Rsp [6]);
  (* (Rhp++)) = (Wrd16.Obj);
  (* (Rhp++)) = (Wrd23.Obj);
  (Wrd22.pObj) = (& (Rhp [-2]));
  (Wrd20.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd22.pObj)));
  (Rsp [6]) = (Wrd20.Obj);
  (Wrd27.Obj) = (Rsp [7]);
  (Wrd28.uLng) = (OBJECT_TYPE (Wrd27.Obj));
  if (! ((Wrd28.uLng) == 1))
    goto label_70;
  (Wrd26.pObj) = (OBJECT_ADDRESS (Wrd27.Obj));
  (Wrd24.Obj) = ((Wrd26.pObj) [1]);

DEFLABEL (label_69)
  (Rsp [7]) = (Wrd24.Obj);
  Rsp = (& (Rsp [4]));
  goto loop_41;

DEFLABEL (label_70)
  (Wrd32.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_16_27]))));
  (* (--Rsp)) = (Wrd32.Obj);
  (* (--Rsp)) = (Wrd27.Obj);
  INVOKE_PRIMITIVE ((current_block [OBJECT_16_3]), 1);

DEFLABEL (label_55)
  (Wrd24.Obj) = Rvl;
  goto label_69;

DEFLABEL (label_73)
  (Wrd54.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_16_24]))));
  (* (--Rsp)) = (Wrd54.Obj);
  (* (--Rsp)) = (Wrd38.Obj);
  INVOKE_PRIMITIVE ((current_block [OBJECT_16_2]), 1);

DEFLABEL (label_53)
  (Wrd48.Obj) = Rvl;
  goto label_72;

DEFLABEL (label_75)
  (Wrd46.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_16_23]))));
  (* (--Rsp)) = (Wrd46.Obj);
  (* (--Rsp)) = (Wrd41.Obj);
  INVOKE_PRIMITIVE ((current_block [OBJECT_16_3]), 1);

DEFLABEL (label_52)
  (Wrd38.Obj) = Rvl;
  goto label_74;

DEFLABEL (label_76)
  (Wrd61.Obj) = (current_block [OBJECT_16_4]);
  Rsp = (& (Rsp [1]));
  (* (--Rsp)) = (Wrd61.Obj);
  goto label_71;

DEFLABEL (label_78)
  (Wrd36.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_16_22]))));
  (* (--Rsp)) = (Wrd36.Obj);
  (* (--Rsp)) = (Wrd31.Obj);
  INVOKE_PRIMITIVE ((current_block [OBJECT_16_3]), 1);

DEFLABEL (label_51)
  (Wrd28.Obj) = Rvl;
  goto label_77;

DEFLABEL (label_80)
  (Wrd24.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_16_21]))));
  (* (--Rsp)) = (Wrd24.Obj);
  (* (--Rsp)) = (Wrd19.Obj);
  INVOKE_PRIMITIVE ((current_block [OBJECT_16_2]), 1);

DEFLABEL (label_50)
  (* (--Rsp)) = Rvl;
  goto label_79;

DEFLABEL (label_82)
  (Wrd15.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_16_20]))));
  (* (--Rsp)) = (Wrd15.Obj);
  (* (--Rsp)) = (Wrd5.Obj);
  INVOKE_PRIMITIVE ((current_block [OBJECT_16_2]), 1);

DEFLABEL (label_49)
  (* (--Rsp)) = Rvl;
  goto label_81;

DEFLABEL (label_83)
  (Wrd64.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_16_18]))));
  (* (--Rsp)) = (Wrd64.Obj);
  (Wrd65.Obj) = (Rsp [6]);
  (* (--Rsp)) = (Wrd65.Obj);
  (Wrd67.pObj) = ((SCHEME_OBJECT *) (current_block [FREE_REFERENCE_16_1]));
  (Wrd70.Obj) = ((Wrd67.pObj) [0]);
  (Wrd71.uLng) = (OBJECT_TYPE (Wrd70.Obj));
  if ((Wrd71.uLng) == 50)
    goto label_85;
  Wrd66 = Wrd70;

DEFLABEL (label_84)
  (* (--Rsp)) = (Wrd66.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_16_13]));

DEFLABEL (continuation_36)
  INTERRUPT_CHECK (27, LABEL_16_18);
  (Wrd9.Obj) = (current_block [OBJECT_16_7]);
  (* (Rhp++)) = (Wrd9.Obj);
  (* (Rhp++)) = Rvl;
  (Wrd8.pObj) = (& (Rhp [-2]));
  (Wrd6.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd8.pObj)));
  (Wrd13.Obj) = (current_block [OBJECT_16_1]);
  (* (Rhp++)) = (Wrd6.Obj);
  (* (Rhp++)) = (Wrd13.Obj);
  (Wrd12.pObj) = (& (Rhp [-2]));
  (Wrd10.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd12.pObj)));
  (Wrd17.Obj) = (current_block [OBJECT_16_8]);
  (Wrd18.Obj) = (Rsp [2]);
  (* (Rhp++)) = (Wrd17.Obj);
  (* (Rhp++)) = (Wrd18.Obj);
  (Wrd16.pObj) = (& (Rhp [-2]));
  (Wrd14.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd16.pObj)));
  (* (Rhp++)) = (Wrd14.Obj);
  (* (Rhp++)) = (Wrd10.Obj);
  (Wrd21.pObj) = (& (Rhp [-2]));
  (Wrd19.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd21.pObj)));
  (Wrd25.Obj) = (current_block [OBJECT_16_9]);
  (Wrd26.Obj) = (Rsp [1]);
  (* (Rhp++)) = (Wrd25.Obj);
  (* (Rhp++)) = (Wrd26.Obj);
  (Wrd24.pObj) = (& (Rhp [-2]));
  (Wrd22.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd24.pObj)));
  (* (Rhp++)) = (Wrd22.Obj);
  (* (Rhp++)) = (Wrd19.Obj);
  (Wrd29.pObj) = (& (Rhp [-2]));
  (Wrd27.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd29.pObj)));
  (Wrd33.Obj) = (current_block [OBJECT_16_10]);
  (Wrd34.Obj) = (Rsp [0]);
  (* (Rhp++)) = (Wrd33.Obj);
  (* (Rhp++)) = (Wrd34.Obj);
  (Wrd32.pObj) = (& (Rhp [-2]));
  (Wrd30.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd32.pObj)));
  (* (Rhp++)) = (Wrd30.Obj);
  (* (Rhp++)) = (Wrd27.Obj);
  (Wrd37.pObj) = (& (Rhp [-2]));
  Rvl = (MAKE_POINTER_OBJECT (1, (Wrd37.pObj)));
  Rsp = (& (Rsp [7]));
  goto pop_return;

DEFLABEL (label_85)
  INVOKE_INTERFACE_2 (31, (& (current_block [LABEL_16_19])), (Wrd67.pObj));

DEFLABEL (label_54)
  (Wrd66.Obj) = Rvl;
  goto label_84;

DEFLABEL (continuation_11)
  INTERRUPT_CHECK (27, LABEL_16_16);
  (* (--Rsp)) = Rvl;
  goto label_71;

INVOKE_INTERFACE_TARGET_1
INVOKE_PRIMITIVE_TARGET
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_17_4 3
#define LABEL_17_6 5
#define LABEL_17_5 7
#define LABEL_17_9 9
#define LABEL_17_8 11
#define LABEL_17_12 13
#define LABEL_17_11 15
#define LABEL_17_14 17
#define LABEL_17_22 19
#define LABEL_17_21 21
#define LABEL_17_16 23
#define LABEL_17_17 25
#define LABEL_17_18 27
#define TAG_17_19 12
#define LABEL_17_24 29
#define LABEL_17_25 31
#define LABEL_17_26 33
#define TAG_17_27 15
#define ENVIRONMENT_LABEL_17_3 66
#define DEBUGGING_LABEL_17_2 65
#define OBJECT_17_11 64
#define OBJECT_17_10 63
#define OBJECT_17_9 62
#define OBJECT_17_8 61
#define OBJECT_17_7 60
#define OBJECT_17_6 59
#define OBJECT_17_5 58
#define OBJECT_17_4 57
#define OBJECT_17_3 56
#define OBJECT_17_2 55
#define OBJECT_17_1 54
#define OBJECT_17_0 53
#define EXECUTE_CACHE_17_29 35
#define EXECUTE_CACHE_17_28 37
#define EXECUTE_CACHE_17_23 39
#define EXECUTE_CACHE_17_20 41
#define EXECUTE_CACHE_17_15 43
#define EXECUTE_CACHE_17_13 45
#define EXECUTE_CACHE_17_10 47
#define EXECUTE_CACHE_17_7 49
#define FREE_REFERENCE_17_0 52
#define FREE_REFERENCES_LABEL_17_0 34
#define NUMBER_OF_LINKER_SECTIONS_17_1 2

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_17 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  machine_word Wrd5;
  machine_word Wrd29;
  machine_word Wrd21;
  machine_word Wrd17;
  machine_word Wrd6;
  machine_word Wrd25;
  machine_word Wrd79;
  machine_word Wrd24;
  machine_word Wrd26;
  machine_word Wrd27;
  machine_word Wrd20;
  machine_word Wrd22;
  machine_word Wrd23;
  machine_word Wrd18;
  machine_word Wrd19;
  machine_word Wrd36;
  machine_word Wrd35;
  machine_word Wrd50;
  machine_word Wrd52;
  machine_word Wrd53;
  machine_word Wrd46;
  machine_word Wrd48;
  machine_word Wrd49;
  machine_word Wrd42;
  machine_word Wrd44;
  machine_word Wrd45;
  machine_word Wrd37;
  machine_word Wrd39;
  machine_word Wrd41;
  machine_word Wrd40;
  machine_word Wrd72;
  machine_word Wrd74;
  machine_word Wrd75;
  machine_word Wrd68;
  machine_word Wrd70;
  machine_word Wrd71;
  machine_word Wrd64;
  machine_word Wrd66;
  machine_word Wrd67;
  machine_word Wrd59;
  machine_word Wrd61;
  machine_word Wrd63;
  machine_word Wrd62;
  machine_word Wrd11;
  machine_word Wrd16;
  machine_word Wrd15;
  machine_word Wrd12;
  machine_word Wrd9;
  machine_word Wrd14;
  machine_word Wrd13;
  machine_word Wrd10;
  machine_word Wrd8;
  machine_word Wrd7;
  INVOKE_INTERFACE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_17_4);
      goto geiser_symbol_documentation_37;

    case 1:
      current_block = (Rpc - LABEL_17_6);
      goto label_39;

    case 2:
      current_block = (Rpc - LABEL_17_5);
      goto continuation_0;

    case 3:
      current_block = (Rpc - LABEL_17_9);
      goto label_40;

    case 4:
      current_block = (Rpc - LABEL_17_8);
      goto continuation_2;

    case 5:
      current_block = (Rpc - LABEL_17_12);
      goto label_41;

    case 6:
      current_block = (Rpc - LABEL_17_11);
      goto continuation_1;

    case 7:
      current_block = (Rpc - LABEL_17_14);
      goto continuation_10;

    case 8:
      current_block = (Rpc - LABEL_17_22);
      goto label_42;

    case 9:
      current_block = (Rpc - LABEL_17_21);
      goto continuation_19;

    case 10:
      current_block = (Rpc - LABEL_17_16);
      goto continuation_16;

    case 11:
      current_block = (Rpc - LABEL_17_17);
      goto continuation_15;

    case 12:
      current_block = (Rpc - LABEL_17_18);
      goto lambda_45;

    case 13:
      current_block = (Rpc - LABEL_17_24);
      goto continuation_23;

    case 14:
      current_block = (Rpc - LABEL_17_25);
      goto continuation_22;

    case 15:
      current_block = (Rpc - LABEL_17_26);
      goto lambda_46;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (geiser_symbol_documentation_44)
DEFLABEL (geiser_symbol_documentation_37)
  INTERRUPT_CHECK (26, LABEL_17_4);
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_17_5]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd8.Obj) = (Rsp [1]);
  (* (--Rsp)) = (Wrd8.Obj);
  (Wrd10.pObj) = ((SCHEME_OBJECT *) (current_block [FREE_REFERENCE_17_0]));
  (Wrd13.Obj) = ((Wrd10.pObj) [0]);
  (Wrd14.uLng) = (OBJECT_TYPE (Wrd13.Obj));
  if ((Wrd14.uLng) == 50)
    goto label_60;
  Wrd9 = Wrd13;

DEFLABEL (label_59)
  (* (--Rsp)) = (Wrd9.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_17_7]));

DEFLABEL (continuation_0)
  INTERRUPT_CHECK (27, LABEL_17_5);
  if (! (Rvl == ((SCHEME_OBJECT) 0)))
    goto label_47;
  Rvl = (current_block [OBJECT_17_0]);
  Rsp = (& (Rsp [1]));
  goto pop_return;

DEFLABEL (label_47)
  (Wrd9.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_17_8]))));
  (* (--Rsp)) = (Wrd9.Obj);
  (Wrd10.Obj) = (Rsp [1]);
  (* (--Rsp)) = (Wrd10.Obj);
  (Wrd12.pObj) = ((SCHEME_OBJECT *) (current_block [FREE_REFERENCE_17_0]));
  (Wrd15.Obj) = ((Wrd12.pObj) [0]);
  (Wrd16.uLng) = (OBJECT_TYPE (Wrd15.Obj));
  if ((Wrd16.uLng) == 50)
    goto label_58;
  Wrd11 = Wrd15;

DEFLABEL (label_57)
  (* (--Rsp)) = (Wrd11.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_17_10]));

DEFLABEL (continuation_2)
  INTERRUPT_CHECK (27, LABEL_17_8);
  (* (--Rsp)) = Rvl;
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_17_11]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd8.Obj) = (Rsp [2]);
  (* (--Rsp)) = (Wrd8.Obj);
  (Wrd10.pObj) = ((SCHEME_OBJECT *) (current_block [FREE_REFERENCE_17_0]));
  (Wrd13.Obj) = ((Wrd10.pObj) [0]);
  (Wrd14.uLng) = (OBJECT_TYPE (Wrd13.Obj));
  if ((Wrd14.uLng) == 50)
    goto label_56;
  Wrd9 = Wrd13;

DEFLABEL (label_55)
  (* (--Rsp)) = (Wrd9.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_17_13]));

DEFLABEL (continuation_1)
  INTERRUPT_CHECK (27, LABEL_17_11);
  (* (--Rsp)) = Rvl;
  if (Rvl == (current_block [OBJECT_17_1]))
    goto label_54;
  if (Rvl == (current_block [OBJECT_17_5]))
    goto label_53;
  if (! (Rvl == (current_block [OBJECT_17_7])))
    goto label_51;
  (Wrd35.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_17_14]))));
  (* (--Rsp)) = (Wrd35.Obj);
  (Wrd36.Obj) = (Rsp [2]);
  (* (--Rsp)) = (Wrd36.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_17_15]));

DEFLABEL (continuation_10)
  INTERRUPT_CHECK (27, LABEL_17_14);
  if (! (Rvl == ((SCHEME_OBJECT) 0)))
    goto label_48;
  (Wrd19.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_17_16]))));
  (* (--Rsp)) = (Wrd19.Obj);
  (Wrd22.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_17_17]))));
  (* (--Rsp)) = (Wrd22.Obj);
  (* (Rhp++)) = (MAKE_OBJECT (TC_MANIFEST_CLOSURE, 4));
  (Wrd24.pObj) = (Rhp + 1);
  Rhp += 1;
  WRITE_LABEL_DESCRIPTOR (Rhp, 0x101, 2);
  (* (Rhp++)) = (dispatch_base + TAG_17_19);
  (* (Rhp++)) = ((SCHEME_OBJECT) (& (current_block [LABEL_17_18])));
  Rhp += 1;
  (Wrd23.Obj) = (MAKE_POINTER_OBJECT (40, (Wrd24.pObj)));
  Wrd25 = Wrd24;
  (Wrd26.Obj) = (Rsp [3]);
  ((Wrd25.pObj) [2]) = (Wrd26.Obj);
  (* (--Rsp)) = (Wrd23.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_17_20]));

DEFLABEL (continuation_15)
  INTERRUPT_CHECK (27, LABEL_17_17);
  (* (--Rsp)) = Rvl;
  (Wrd5.Obj) = (current_block [OBJECT_17_10]);
  (* (--Rsp)) = (Wrd5.Obj);
  (* (--Rsp)) = ((SCHEME_OBJECT) 0);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_17_28]));

DEFLABEL (continuation_16)
  INTERRUPT_CHECK (27, LABEL_17_16);
  (Wrd9.Obj) = (current_block [OBJECT_17_9]);
  (* (Rhp++)) = (Wrd9.Obj);
  (* (Rhp++)) = Rvl;
  (Wrd8.pObj) = (& (Rhp [-2]));
  (Wrd6.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd8.pObj)));
  (Wrd13.Obj) = (current_block [OBJECT_17_0]);
  (* (Rhp++)) = (Wrd6.Obj);
  (* (Rhp++)) = (Wrd13.Obj);
  (Wrd12.pObj) = (& (Rhp [-2]));
  (Wrd10.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd12.pObj)));
  (Wrd17.Obj) = (current_block [OBJECT_17_2]);
  (* (Rhp++)) = (Wrd17.Obj);
  (* (Rhp++)) = (Wrd13.Obj);
  (Wrd16.pObj) = (& (Rhp [-2]));
  (Wrd14.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd16.pObj)));
  (Wrd22.Obj) = (Rsp [2]);
  (* (Rhp++)) = (Wrd22.Obj);
  (* (Rhp++)) = (Wrd14.Obj);
  (Wrd21.pObj) = (& (Rhp [-2]));
  (Wrd19.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd21.pObj)));
  (Wrd26.Obj) = (current_block [OBJECT_17_3]);
  (* (Rhp++)) = (Wrd26.Obj);
  (* (Rhp++)) = (Wrd19.Obj);
  (Wrd25.pObj) = (& (Rhp [-2]));
  (Wrd23.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd25.pObj)));
  (* (Rhp++)) = (Wrd23.Obj);
  (* (Rhp++)) = (Wrd10.Obj);
  (Wrd29.pObj) = (& (Rhp [-2]));
  Rvl = (MAKE_POINTER_OBJECT (1, (Wrd29.pObj)));
  Rsp = (& (Rsp [3]));
  goto pop_return;

DEFLABEL (label_48)
  (Wrd9.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_17_21]))));
  (* (--Rsp)) = (Wrd9.Obj);
  (Wrd11.pObj) = ((SCHEME_OBJECT *) (current_block [FREE_REFERENCE_17_0]));
  (Wrd14.Obj) = ((Wrd11.pObj) [0]);
  (Wrd15.uLng) = (OBJECT_TYPE (Wrd14.Obj));
  if ((Wrd15.uLng) == 50)
    goto label_50;
  Wrd10 = Wrd14;

DEFLABEL (label_49)
  (* (--Rsp)) = (Wrd10.Obj);
  (Wrd16.Obj) = (Rsp [4]);
  (* (--Rsp)) = (Wrd16.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_17_23]));

DEFLABEL (continuation_19)
  INTERRUPT_CHECK (27, LABEL_17_21);
  (* (--Rsp)) = Rvl;
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_17_24]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd10.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_17_25]))));
  (* (--Rsp)) = (Wrd10.Obj);
  (* (Rhp++)) = (MAKE_OBJECT (TC_MANIFEST_CLOSURE, 4));
  (Wrd12.pObj) = (Rhp + 1);
  Rhp += 1;
  WRITE_LABEL_DESCRIPTOR (Rhp, 0x101, 2);
  (* (Rhp++)) = (dispatch_base + TAG_17_27);
  (* (Rhp++)) = ((SCHEME_OBJECT) (& (current_block [LABEL_17_26])));
  Rhp += 1;
  (Wrd11.Obj) = (MAKE_POINTER_OBJECT (40, (Wrd12.pObj)));
  Wrd13 = Wrd12;
  (Wrd14.Obj) = (Rsp [4]);
  ((Wrd13.pObj) [2]) = (Wrd14.Obj);
  (* (--Rsp)) = (Wrd11.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_17_20]));

DEFLABEL (continuation_22)
  INTERRUPT_CHECK (27, LABEL_17_25);
  (* (--Rsp)) = Rvl;
  (Wrd5.Obj) = (current_block [OBJECT_17_11]);
  (* (--Rsp)) = (Wrd5.Obj);
  (* (--Rsp)) = ((SCHEME_OBJECT) 0);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_17_28]));

DEFLABEL (continuation_23)
  INTERRUPT_CHECK (27, LABEL_17_24);
  (Wrd9.Obj) = (current_block [OBJECT_17_9]);
  (* (Rhp++)) = (Wrd9.Obj);
  (* (Rhp++)) = Rvl;
  (Wrd8.pObj) = (& (Rhp [-2]));
  (Wrd6.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd8.pObj)));
  (Wrd13.Obj) = (current_block [OBJECT_17_0]);
  (* (Rhp++)) = (Wrd6.Obj);
  (* (Rhp++)) = (Wrd13.Obj);
  (Wrd12.pObj) = (& (Rhp [-2]));
  (Wrd10.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd12.pObj)));
  (Wrd17.Obj) = (current_block [OBJECT_17_3]);
  (Wrd18.Obj) = (Rsp [0]);
  (* (Rhp++)) = (Wrd17.Obj);
  (* (Rhp++)) = (Wrd18.Obj);
  (Wrd16.pObj) = (& (Rhp [-2]));
  (Wrd14.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd16.pObj)));
  (* (Rhp++)) = (Wrd14.Obj);
  (* (Rhp++)) = (Wrd10.Obj);
  (Wrd21.pObj) = (& (Rhp [-2]));
  Rvl = (MAKE_POINTER_OBJECT (1, (Wrd21.pObj)));
  Rsp = (& (Rsp [4]));
  goto pop_return;

DEFLABEL (label_50)
  INVOKE_INTERFACE_2 (31, (& (current_block [LABEL_17_22])), (Wrd11.pObj));

DEFLABEL (label_42)
  (Wrd10.Obj) = Rvl;
  goto label_49;

DEFLABEL (label_51)
  (Wrd14.Obj) = (current_block [OBJECT_17_2]);
  (Wrd15.Obj) = (current_block [OBJECT_17_0]);
  (* (Rhp++)) = (Wrd14.Obj);
  (* (Rhp++)) = (Wrd15.Obj);
  (Wrd13.pObj) = (& (Rhp [-2]));
  (Wrd11.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd13.pObj)));
  (Wrd19.Obj) = (Rsp [2]);
  (* (Rhp++)) = (Wrd19.Obj);
  (* (Rhp++)) = (Wrd11.Obj);
  (Wrd18.pObj) = (& (Rhp [-2]));
  (Wrd16.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd18.pObj)));
  (Wrd23.Obj) = (current_block [OBJECT_17_3]);
  (* (Rhp++)) = (Wrd23.Obj);
  (* (Rhp++)) = (Wrd16.Obj);
  (Wrd22.pObj) = (& (Rhp [-2]));
  (Wrd20.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd22.pObj)));
  (Wrd27.Obj) = (current_block [OBJECT_17_8]);
  (* (Rhp++)) = (Wrd27.Obj);
  (* (Rhp++)) = (Wrd15.Obj);
  (Wrd26.pObj) = (& (Rhp [-2]));
  (Wrd24.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd26.pObj)));
  (* (Rhp++)) = (Wrd20.Obj);
  (* (Rhp++)) = (Wrd24.Obj);

DEFLABEL (label_52)
  (Wrd79.pObj) = (& (Rhp [-2]));
  Rvl = (MAKE_POINTER_OBJECT (1, (Wrd79.pObj)));
  Rsp = (& (Rsp [3]));
  goto pop_return;

DEFLABEL (label_53)
  (Wrd40.Obj) = (current_block [OBJECT_17_2]);
  (Wrd41.Obj) = (current_block [OBJECT_17_0]);
  (* (Rhp++)) = (Wrd40.Obj);
  (* (Rhp++)) = (Wrd41.Obj);
  (Wrd39.pObj) = (& (Rhp [-2]));
  (Wrd37.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd39.pObj)));
  (Wrd45.Obj) = (Rsp [2]);
  (* (Rhp++)) = (Wrd45.Obj);
  (* (Rhp++)) = (Wrd37.Obj);
  (Wrd44.pObj) = (& (Rhp [-2]));
  (Wrd42.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd44.pObj)));
  (Wrd49.Obj) = (current_block [OBJECT_17_3]);
  (* (Rhp++)) = (Wrd49.Obj);
  (* (Rhp++)) = (Wrd42.Obj);
  (Wrd48.pObj) = (& (Rhp [-2]));
  (Wrd46.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd48.pObj)));
  (Wrd53.Obj) = (current_block [OBJECT_17_6]);
  (* (Rhp++)) = (Wrd53.Obj);
  (* (Rhp++)) = (Wrd41.Obj);
  (Wrd52.pObj) = (& (Rhp [-2]));
  (Wrd50.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd52.pObj)));
  (* (Rhp++)) = (Wrd46.Obj);
  (* (Rhp++)) = (Wrd50.Obj);
  goto label_52;

DEFLABEL (label_54)
  (Wrd62.Obj) = (current_block [OBJECT_17_2]);
  (Wrd63.Obj) = (current_block [OBJECT_17_0]);
  (* (Rhp++)) = (Wrd62.Obj);
  (* (Rhp++)) = (Wrd63.Obj);
  (Wrd61.pObj) = (& (Rhp [-2]));
  (Wrd59.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd61.pObj)));
  (Wrd67.Obj) = (Rsp [2]);
  (* (Rhp++)) = (Wrd67.Obj);
  (* (Rhp++)) = (Wrd59.Obj);
  (Wrd66.pObj) = (& (Rhp [-2]));
  (Wrd64.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd66.pObj)));
  (Wrd71.Obj) = (current_block [OBJECT_17_3]);
  (* (Rhp++)) = (Wrd71.Obj);
  (* (Rhp++)) = (Wrd64.Obj);
  (Wrd70.pObj) = (& (Rhp [-2]));
  (Wrd68.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd70.pObj)));
  (Wrd75.Obj) = (current_block [OBJECT_17_4]);
  (* (Rhp++)) = (Wrd75.Obj);
  (* (Rhp++)) = (Wrd63.Obj);
  (Wrd74.pObj) = (& (Rhp [-2]));
  (Wrd72.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd74.pObj)));
  (* (Rhp++)) = (Wrd68.Obj);
  (* (Rhp++)) = (Wrd72.Obj);
  goto label_52;

DEFLABEL (label_56)
  INVOKE_INTERFACE_2 (31, (& (current_block [LABEL_17_12])), (Wrd10.pObj));

DEFLABEL (label_41)
  (Wrd9.Obj) = Rvl;
  goto label_55;

DEFLABEL (label_58)
  INVOKE_INTERFACE_2 (31, (& (current_block [LABEL_17_9])), (Wrd12.pObj));

DEFLABEL (label_40)
  (Wrd11.Obj) = Rvl;
  goto label_57;

DEFLABEL (label_60)
  INVOKE_INTERFACE_2 (31, (& (current_block [LABEL_17_6])), (Wrd10.pObj));

DEFLABEL (label_39)
  (Wrd9.Obj) = Rvl;
  goto label_59;

DEFLABEL (lambda_45)
  CLOSURE_HEADER (LABEL_17_18);

DEFLABEL (lambda_14)
  CLOSURE_INTERRUPT_CHECK (24);
  (Wrd6.Obj) = (Rsp [0]);
  (Wrd7.pObj) = (OBJECT_ADDRESS (Wrd6.Obj));
  (Wrd5.Obj) = ((Wrd7.pObj) [2]);
  (Rsp [0]) = (Wrd5.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_17_29]));

DEFLABEL (lambda_46)
  CLOSURE_HEADER (LABEL_17_26);

DEFLABEL (lambda_21)
  CLOSURE_INTERRUPT_CHECK (24);
  (Wrd6.Obj) = (Rsp [0]);
  (Wrd7.pObj) = (OBJECT_ADDRESS (Wrd6.Obj));
  (Wrd5.Obj) = ((Wrd7.pObj) [2]);
  (Rsp [0]) = (Wrd5.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_17_29]));

INVOKE_INTERFACE_TARGET_0
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_18_4 3
#define LABEL_18_6 5
#define LABEL_18_5 7
#define LABEL_18_9 9
#define LABEL_18_8 11
#define LABEL_18_12 13
#define LABEL_18_11 15
#define LABEL_18_14 17
#define ENVIRONMENT_LABEL_18_3 36
#define DEBUGGING_LABEL_18_2 35
#define OBJECT_18_5 34
#define OBJECT_18_4 33
#define OBJECT_18_3 32
#define OBJECT_18_2 31
#define OBJECT_18_1 30
#define OBJECT_18_0 29
#define EXECUTE_CACHE_18_15 19
#define EXECUTE_CACHE_18_13 21
#define EXECUTE_CACHE_18_10 23
#define EXECUTE_CACHE_18_7 25
#define FREE_REFERENCE_18_0 28
#define FREE_REFERENCES_LABEL_18_0 18
#define NUMBER_OF_LINKER_SECTIONS_18_1 2

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_18 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  machine_word Wrd23;
  machine_word Wrd18;
  machine_word Wrd6;
  machine_word Wrd11;
  machine_word Wrd16;
  machine_word Wrd15;
  machine_word Wrd12;
  machine_word Wrd33;
  machine_word Wrd27;
  machine_word Wrd29;
  machine_word Wrd30;
  machine_word Wrd22;
  machine_word Wrd24;
  machine_word Wrd26;
  machine_word Wrd25;
  machine_word Wrd17;
  machine_word Wrd19;
  machine_word Wrd21;
  machine_word Wrd20;
  machine_word Wrd9;
  machine_word Wrd14;
  machine_word Wrd13;
  machine_word Wrd10;
  machine_word Wrd8;
  machine_word Wrd7;
  INVOKE_INTERFACE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_18_4);
      goto geiser_symbol_location_14;

    case 1:
      current_block = (Rpc - LABEL_18_6);
      goto label_16;

    case 2:
      current_block = (Rpc - LABEL_18_5);
      goto continuation_0;

    case 3:
      current_block = (Rpc - LABEL_18_9);
      goto label_17;

    case 4:
      current_block = (Rpc - LABEL_18_8);
      goto continuation_5;

    case 5:
      current_block = (Rpc - LABEL_18_12);
      goto label_18;

    case 6:
      current_block = (Rpc - LABEL_18_11);
      goto continuation_4;

    case 7:
      current_block = (Rpc - LABEL_18_14);
      goto continuation_7;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (geiser_symbol_location_20)
DEFLABEL (geiser_symbol_location_14)
  INTERRUPT_CHECK (26, LABEL_18_4);
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_18_5]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd8.Obj) = (Rsp [1]);
  (* (--Rsp)) = (Wrd8.Obj);
  (Wrd10.pObj) = ((SCHEME_OBJECT *) (current_block [FREE_REFERENCE_18_0]));
  (Wrd13.Obj) = ((Wrd10.pObj) [0]);
  (Wrd14.uLng) = (OBJECT_TYPE (Wrd13.Obj));
  if ((Wrd14.uLng) == 50)
    goto label_28;
  Wrd9 = Wrd13;

DEFLABEL (label_27)
  (* (--Rsp)) = (Wrd9.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_18_7]));

DEFLABEL (continuation_0)
  INTERRUPT_CHECK (27, LABEL_18_5);
  if (! (Rvl == ((SCHEME_OBJECT) 0)))
    goto label_21;
  (Wrd20.Obj) = (current_block [OBJECT_18_0]);
  (Wrd21.Obj) = (Rsp [0]);
  (* (Rhp++)) = (Wrd20.Obj);
  (* (Rhp++)) = (Wrd21.Obj);
  (Wrd19.pObj) = (& (Rhp [-2]));
  (Wrd17.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd19.pObj)));
  (Wrd25.Obj) = (current_block [OBJECT_18_1]);
  (Wrd26.Obj) = (current_block [OBJECT_18_2]);
  (* (Rhp++)) = (Wrd25.Obj);
  (* (Rhp++)) = (Wrd26.Obj);
  (Wrd24.pObj) = (& (Rhp [-2]));
  (Wrd22.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd24.pObj)));
  (Wrd30.Obj) = (current_block [OBJECT_18_3]);
  (* (Rhp++)) = (Wrd30.Obj);
  (* (Rhp++)) = (Wrd22.Obj);
  (Wrd29.pObj) = (& (Rhp [-2]));
  (Wrd27.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd29.pObj)));
  (* (Rhp++)) = (Wrd17.Obj);
  (* (Rhp++)) = (Wrd27.Obj);
  (Wrd33.pObj) = (& (Rhp [-2]));
  Rvl = (MAKE_POINTER_OBJECT (1, (Wrd33.pObj)));
  Rsp = (& (Rsp [1]));
  goto pop_return;

DEFLABEL (label_21)
  (Wrd9.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_18_8]))));
  (* (--Rsp)) = (Wrd9.Obj);
  (Wrd10.Obj) = (Rsp [1]);
  (* (--Rsp)) = (Wrd10.Obj);
  (Wrd12.pObj) = ((SCHEME_OBJECT *) (current_block [FREE_REFERENCE_18_0]));
  (Wrd15.Obj) = ((Wrd12.pObj) [0]);
  (Wrd16.uLng) = (OBJECT_TYPE (Wrd15.Obj));
  if ((Wrd16.uLng) == 50)
    goto label_26;
  Wrd11 = Wrd15;

DEFLABEL (label_25)
  (* (--Rsp)) = (Wrd11.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_18_10]));

DEFLABEL (continuation_5)
  INTERRUPT_CHECK (27, LABEL_18_8);
  (* (--Rsp)) = Rvl;
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_18_11]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd8.Obj) = (Rsp [2]);
  (* (--Rsp)) = (Wrd8.Obj);
  (Wrd10.pObj) = ((SCHEME_OBJECT *) (current_block [FREE_REFERENCE_18_0]));
  (Wrd13.Obj) = ((Wrd10.pObj) [0]);
  (Wrd14.uLng) = (OBJECT_TYPE (Wrd13.Obj));
  if ((Wrd14.uLng) == 50)
    goto label_24;
  Wrd9 = Wrd13;

DEFLABEL (label_23)
  (* (--Rsp)) = (Wrd9.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_18_13]));

DEFLABEL (continuation_4)
  INTERRUPT_CHECK (27, LABEL_18_11);
  (* (--Rsp)) = Rvl;
  if (Rvl == (current_block [OBJECT_18_4]))
    goto label_22;
  Rvl = (current_block [OBJECT_18_2]);
  Rsp = (& (Rsp [3]));
  goto pop_return;

DEFLABEL (label_22)
  (Wrd11.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_18_14]))));
  (* (--Rsp)) = (Wrd11.Obj);
  (Wrd12.Obj) = (Rsp [2]);
  (* (--Rsp)) = (Wrd12.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_18_15]));

DEFLABEL (continuation_7)
  INTERRUPT_CHECK (27, LABEL_18_14);
  (Rsp [1]) = Rvl;
  Rsp = (& (Rsp [1]));
  (Wrd9.Obj) = (current_block [OBJECT_18_0]);
  (Wrd10.Obj) = (Rsp [1]);
  (* (Rhp++)) = (Wrd9.Obj);
  (* (Rhp++)) = (Wrd10.Obj);
  (Wrd8.pObj) = (& (Rhp [-2]));
  (Wrd6.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd8.pObj)));
  (Wrd14.Obj) = (current_block [OBJECT_18_5]);
  (Wrd15.Obj) = (Rsp [0]);
  (* (Rhp++)) = (Wrd14.Obj);
  (* (Rhp++)) = (Wrd15.Obj);
  (Wrd13.pObj) = (& (Rhp [-2]));
  (Wrd11.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd13.pObj)));
  (Wrd19.Obj) = (current_block [OBJECT_18_1]);
  (Wrd20.Obj) = (current_block [OBJECT_18_2]);
  (* (Rhp++)) = (Wrd19.Obj);
  (* (Rhp++)) = (Wrd20.Obj);
  (Wrd18.pObj) = (& (Rhp [-2]));
  (Wrd16.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd18.pObj)));
  (* (Rhp++)) = (Wrd11.Obj);
  (* (Rhp++)) = (Wrd16.Obj);
  (Wrd23.pObj) = (& (Rhp [-2]));
  (Wrd21.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd23.pObj)));
  (* (Rhp++)) = (Wrd6.Obj);
  (* (Rhp++)) = (Wrd21.Obj);
  (Wrd26.pObj) = (& (Rhp [-2]));
  Rvl = (MAKE_POINTER_OBJECT (1, (Wrd26.pObj)));
  Rsp = (& (Rsp [2]));
  goto pop_return;

DEFLABEL (label_24)
  INVOKE_INTERFACE_2 (31, (& (current_block [LABEL_18_12])), (Wrd10.pObj));

DEFLABEL (label_18)
  (Wrd9.Obj) = Rvl;
  goto label_23;

DEFLABEL (label_26)
  INVOKE_INTERFACE_2 (31, (& (current_block [LABEL_18_9])), (Wrd12.pObj));

DEFLABEL (label_17)
  (Wrd11.Obj) = Rvl;
  goto label_25;

DEFLABEL (label_28)
  INVOKE_INTERFACE_2 (31, (& (current_block [LABEL_18_6])), (Wrd10.pObj));

DEFLABEL (label_16)
  (Wrd9.Obj) = Rvl;
  goto label_27;

INVOKE_INTERFACE_TARGET_1
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_19_4 3
#define ENVIRONMENT_LABEL_19_3 9
#define DEBUGGING_LABEL_19_2 8
#define OBJECT_19_3 7
#define OBJECT_19_2 6
#define OBJECT_19_1 5
#define OBJECT_19_0 4
#define FREE_REFERENCES_LABEL_19_0 4
#define NUMBER_OF_LINKER_SECTIONS_19_1 0

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_19 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  machine_word Wrd21;
  machine_word Wrd15;
  machine_word Wrd17;
  machine_word Wrd18;
  machine_word Wrd10;
  machine_word Wrd12;
  machine_word Wrd14;
  machine_word Wrd13;
  machine_word Wrd5;
  machine_word Wrd7;
  machine_word Wrd9;
  machine_word Wrd8;
  INVOKE_INTERFACE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_19_4);
      goto geiser_module_location_3;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (geiser_module_location_6)
DEFLABEL (geiser_module_location_3)
  INTERRUPT_CHECK (26, LABEL_19_4);
  (Wrd8.Obj) = (current_block [OBJECT_19_0]);
  (Wrd9.Obj) = (Rsp [0]);
  (* (Rhp++)) = (Wrd8.Obj);
  (* (Rhp++)) = (Wrd9.Obj);
  (Wrd7.pObj) = (& (Rhp [-2]));
  (Wrd5.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd7.pObj)));
  (Wrd13.Obj) = (current_block [OBJECT_19_1]);
  (Wrd14.Obj) = (current_block [OBJECT_19_2]);
  (* (Rhp++)) = (Wrd13.Obj);
  (* (Rhp++)) = (Wrd14.Obj);
  (Wrd12.pObj) = (& (Rhp [-2]));
  (Wrd10.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd12.pObj)));
  (Wrd18.Obj) = (current_block [OBJECT_19_3]);
  (* (Rhp++)) = (Wrd18.Obj);
  (* (Rhp++)) = (Wrd10.Obj);
  (Wrd17.pObj) = (& (Rhp [-2]));
  (Wrd15.Obj) = (MAKE_POINTER_OBJECT (1, (Wrd17.pObj)));
  (* (Rhp++)) = (Wrd5.Obj);
  (* (Rhp++)) = (Wrd15.Obj);
  (Wrd21.pObj) = (& (Rhp [-2]));
  Rvl = (MAKE_POINTER_OBJECT (1, (Wrd21.pObj)));
  Rsp = (& (Rsp [1]));
  goto pop_return;

INVOKE_INTERFACE_TARGET_1
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_20_4 3
#define ENVIRONMENT_LABEL_20_3 5
#define DEBUGGING_LABEL_20_2 4
#define FREE_REFERENCES_LABEL_20_0 4
#define NUMBER_OF_LINKER_SECTIONS_20_1 0

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_20 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  INVOKE_INTERFACE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_20_4);
      goto geiser_newline_0;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (geiser_newline_3)
DEFLABEL (geiser_newline_0)
  INTERRUPT_CHECK (26, LABEL_20_4);
  Rvl = ((SCHEME_OBJECT) 0);
  goto pop_return;

INVOKE_INTERFACE_TARGET_1
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_21_4 3
#define ENVIRONMENT_LABEL_21_3 5
#define DEBUGGING_LABEL_21_2 4
#define FREE_REFERENCES_LABEL_21_0 4
#define NUMBER_OF_LINKER_SECTIONS_21_1 0

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_21 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  INVOKE_INTERFACE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_21_4);
      goto geiser_no_values_0;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (geiser_no_values_3)
DEFLABEL (geiser_no_values_0)
  INTERRUPT_CHECK (26, LABEL_21_4);
  Rvl = ((SCHEME_OBJECT) 0);
  goto pop_return;

INVOKE_INTERFACE_TARGET_1
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_22_4 3
#define LABEL_22_5 5
#define LABEL_22_6 7
#define ENVIRONMENT_LABEL_22_3 13
#define DEBUGGING_LABEL_22_2 12
#define FREE_REFERENCE_22_0 9
#define FREE_ASSIGNMENT_22_0 11
#define FREE_REFERENCES_LABEL_22_0 8
#define NUMBER_OF_LINKER_SECTIONS_22_1 2

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_22 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  machine_word Wrd26;
  machine_word Wrd25;
  machine_word Wrd18;
  machine_word Wrd17;
  machine_word Wrd5;
  machine_word Wrd10;
  machine_word Wrd9;
  machine_word Wrd6;
  INVOKE_INTERFACE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_22_4);
      goto geiser_set_mit_scheme_source_directory_2;

    case 1:
      current_block = (Rpc - LABEL_22_5);
      goto label_4;

    case 2:
      current_block = (Rpc - LABEL_22_6);
      goto label_5;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (geiser_set_mit_scheme_source_directory_7)
DEFLABEL (geiser_set_mit_scheme_source_directory_2)
  INTERRUPT_CHECK (26, LABEL_22_4);
  (Wrd6.pObj) = ((SCHEME_OBJECT *) (current_block [FREE_REFERENCE_22_0]));
  (Wrd9.Obj) = ((Wrd6.pObj) [0]);
  (Wrd10.uLng) = (OBJECT_TYPE (Wrd9.Obj));
  if ((Wrd10.uLng) == 50)
    goto label_13;

DEFLABEL (label_12)
  Wrd5 = Wrd9;

DEFLABEL (label_11)
  (* (--Rsp)) = (Wrd5.Obj);
  (Wrd17.pObj) = ((SCHEME_OBJECT *) (current_block [FREE_ASSIGNMENT_22_0]));
  (Wrd18.Obj) = (Rsp [1]);
  (Wrd25.Obj) = ((Wrd17.pObj) [0]);
  (Wrd26.uLng) = (OBJECT_TYPE (Wrd25.Obj));
  if ((Wrd26.uLng) == 50)
    goto label_10;

DEFLABEL (label_9)
  ((Wrd17.pObj) [0]) = (Wrd18.Obj);

DEFLABEL (label_8)
  Rvl = (Rsp [0]);
  Rsp = (& (Rsp [2]));
  goto pop_return;

DEFLABEL (label_10)
  if ((Wrd25.Obj) == (MAKE_OBJECT (50, 0)))
    goto label_9;
  INVOKE_INTERFACE_3 (29, (& (current_block [LABEL_22_6])), (Wrd17.pObj), (Wrd18.Obj));

DEFLABEL (label_5)
  goto label_8;

DEFLABEL (label_13)
  if ((Wrd9.Obj) == (MAKE_OBJECT (50, 0)))
    goto label_12;
  INVOKE_INTERFACE_2 (32, (& (current_block [LABEL_22_5])), (Wrd6.pObj));

DEFLABEL (label_4)
  (Wrd5.Obj) = Rvl;
  goto label_11;

INVOKE_INTERFACE_TARGET_1
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_23_4 3
#define ENVIRONMENT_LABEL_23_3 5
#define DEBUGGING_LABEL_23_2 4
#define FREE_REFERENCES_LABEL_23_0 4
#define NUMBER_OF_LINKER_SECTIONS_23_1 0

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_23 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  INVOKE_INTERFACE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_23_4);
      goto geiser_callers_0;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (geiser_callers_3)
DEFLABEL (geiser_callers_0)
  INTERRUPT_CHECK (26, LABEL_23_4);
  Rvl = ((SCHEME_OBJECT) 0);
  Rsp = (& (Rsp [1]));
  goto pop_return;

INVOKE_INTERFACE_TARGET_1
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_24_4 3
#define ENVIRONMENT_LABEL_24_3 5
#define DEBUGGING_LABEL_24_2 4
#define FREE_REFERENCES_LABEL_24_0 4
#define NUMBER_OF_LINKER_SECTIONS_24_1 0

#ifndef WANT_ONLY_DATA

static SCHEME_OBJECT *
emacs_so_code_24 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  INVOKE_INTERFACE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_24_4);
      goto geiser_callees_0;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (geiser_callees_3)
DEFLABEL (geiser_callees_0)
  INTERRUPT_CHECK (26, LABEL_24_4);
  Rvl = ((SCHEME_OBJECT) 0);
  Rsp = (& (Rsp [1]));
  goto pop_return;

INVOKE_INTERFACE_TARGET_1
}

#endif /* !WANT_ONLY_DATA */

#define LABEL_4 3
#define LABEL_6 5
#define LABEL_8 7
#define LABEL_9 9
#define LABEL_11 11
#define LABEL_12 13
#define LABEL_13 15
#define LABEL_17 17
#define LABEL_18 19
#define LABEL_19 21
#define ENVIRONMENT_LABEL_3 47
#define DEBUGGING_LABEL_2 46
#define PURIFICATION_ROOT 45
#define OBJECT_8 44
#define OBJECT_7 43
#define OBJECT_6 42
#define OBJECT_5 41
#define OBJECT_4 40
#define OBJECT_3 39
#define OBJECT_2 38
#define OBJECT_1 37
#define OBJECT_0 36
#define EXECUTE_CACHE_16 23
#define EXECUTE_CACHE_15 25
#define EXECUTE_CACHE_14 27
#define EXECUTE_CACHE_10 29
#define EXECUTE_CACHE_5 31
#define GLOBAL_EXECUTE_CACHE_7 34
#define FREE_REFERENCES_LABEL_0 22
#define NUMBER_OF_LINKER_SECTIONS_1 2

#ifndef WANT_ONLY_DATA

SCHEME_OBJECT *
emacs_so_64215321dbe48238 (SCHEME_OBJECT * Rpc, entry_count_t dispatch_base)
{
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES ();
  SCHEME_OBJECT * Rdl ATTRIBUTE((unused));
  machine_word Wrd11;
  machine_word Wrd6;
  machine_word Wrd10;
  machine_word Wrd7;
  machine_word Wrd9;
  machine_word Wrd8;
  machine_word Wrd5;
  INVOKE_INTERFACE_DECLS
  INVOKE_PRIMITIVE_DECLS

  Rdl = (OBJECT_ADDRESS (Rvl));
  goto perform_dispatch;

DEFLABEL (pop_return)
  Rpc = (OBJECT_ADDRESS (* (Rsp++)));

DEFLABEL (perform_dispatch)
  switch ((* ((unsigned long *) Rpc)) - dispatch_base)
    {
    case 0:
      current_block = (Rpc - LABEL_4);
      goto continuation_0;

    case 1:
      current_block = (Rpc - LABEL_6);
      goto continuation_1;

    case 2:
      current_block = (Rpc - LABEL_8);
      goto continuation_3;

    case 3:
      current_block = (Rpc - LABEL_9);
      goto continuation_2;

    case 4:
      current_block = (Rpc - LABEL_11);
      goto continuation_4;

    case 5:
      current_block = (Rpc - LABEL_12);
      goto continuation_6;

    case 6:
      current_block = (Rpc - LABEL_13);
      goto continuation_5;

    case 7:
      current_block = (Rpc - LABEL_17);
      goto label_11;

    case 8:
      current_block = (Rpc - LABEL_18);
      goto label_12;

    case 9:
      current_block = (Rpc - LABEL_19);
      goto expression_8;

    default:
      UNCACHE_VARIABLES ();
      return (Rpc);
    }

DEFLABEL (expression_8)
  (current_block [ENVIRONMENT_LABEL_3]) = (Rrb [REGBLOCK_ENV]);
  INVOKE_INTERFACE_4 (23, (& (current_block [LABEL_18])), current_block, (& (current_block [FREE_REFERENCES_LABEL_0])), NUMBER_OF_LINKER_SECTIONS_1);

DEFLABEL (label_12)
  (* (--Rsp)) = (ULONG_TO_FIXNUM (1UL));

DEFLABEL (label_11)
  {
    static const short sections [] =
      {
	0,
	0,
	0,
	2,
	0,
	0,
	0,
	2,
	2,
	2,
	1,
	2,
	2,
	1,
	1,
	1,
	2,
	1,
	2,
	2,
	1,
	2,
	1,
	1,
	1
      };
    unsigned long counter = (OBJECT_DATUM (* Rsp));
    SCHEME_OBJECT blocks;
    SCHEME_OBJECT * sub_block;
    short section;
    if (counter > 24)
      goto label_10;
    blocks = (current_block [OBJECT_8]);
    sub_block = (OBJECT_ADDRESS (MEMORY_REF (blocks, counter)));
    (sub_block [(OBJECT_DATUM (sub_block [0]))]) = (Rrb [REGBLOCK_ENV]);
    section = (sections [counter]);
    (* Rsp) = (ULONG_TO_FIXNUM (counter + 1));
    INVOKE_INTERFACE_4 (23, (& (current_block [LABEL_17])), sub_block, (sub_block + (2 + (OBJECT_DATUM (sub_block [1])))), section);
  }

DEFLABEL (label_10)
  Rsp += 1;
  (Wrd5.Obj) = (Rrb [3]);
  (* (--Rsp)) = (Wrd5.Obj);
  (Wrd8.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_4]))));
  (* (--Rsp)) = (Wrd8.Obj);
  (Wrd9.Obj) = (current_block [OBJECT_0]);
  (* (--Rsp)) = (Wrd9.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_5]));

DEFLABEL (continuation_0)
  INTERRUPT_CHECK (27, LABEL_4);
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_6]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd8.Obj) = (current_block [OBJECT_1]);
  (* (--Rsp)) = (Wrd8.Obj);
  (Wrd9.Obj) = (current_block [OBJECT_2]);
  (* (--Rsp)) = (Wrd9.Obj);
  (Wrd10.Obj) = (Rsp [3]);
  (* (--Rsp)) = (Wrd10.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [GLOBAL_EXECUTE_CACHE_7]));

DEFLABEL (continuation_1)
  INTERRUPT_CHECK (27, LABEL_6);
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_8]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd10.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_9]))));
  (* (--Rsp)) = (Wrd10.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_10]));

DEFLABEL (continuation_2)
  INTERRUPT_CHECK (27, LABEL_9);
  (* (--Rsp)) = Rvl;
  (Wrd5.Obj) = (current_block [OBJECT_5]);
  (* (--Rsp)) = (Wrd5.Obj);
  (Wrd6.Obj) = (Rsp [3]);
  (* (--Rsp)) = (Wrd6.Obj);
  INVOKE_PRIMITIVE ((current_block [OBJECT_6]), 3);

DEFLABEL (continuation_3)
  INTERRUPT_CHECK (27, LABEL_8);
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_11]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd8.Obj) = (current_block [OBJECT_3]);
  (* (--Rsp)) = (Wrd8.Obj);
  (Wrd9.Obj) = (current_block [OBJECT_4]);
  (* (--Rsp)) = (Wrd9.Obj);
  (Wrd10.Obj) = (Rsp [3]);
  (* (--Rsp)) = (Wrd10.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [GLOBAL_EXECUTE_CACHE_7]));

DEFLABEL (continuation_4)
  INTERRUPT_CHECK (27, LABEL_11);
  (Wrd7.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_12]))));
  (* (--Rsp)) = (Wrd7.Obj);
  (Wrd10.Obj) = (MAKE_POINTER_OBJECT (40, (& (current_block [LABEL_13]))));
  (* (--Rsp)) = (Wrd10.Obj);
  (Wrd11.Obj) = (current_block [OBJECT_7]);
  (* (--Rsp)) = (Wrd11.Obj);
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_14]));

DEFLABEL (continuation_5)
  INTERRUPT_CHECK (27, LABEL_13);
  (* (--Rsp)) = Rvl;
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_16]));

DEFLABEL (continuation_6)
  INTERRUPT_CHECK (27, LABEL_12);
  (Rsp [0]) = Rvl;
  JUMP ((SCHEME_OBJECT *) (current_block [EXECUTE_CACHE_15]));

INVOKE_INTERFACE_TARGET_1
INVOKE_PRIMITIVE_TARGET
}

static const struct liarc_code_S arr_decl_emacs_so_64215321dbe48238 [24] =
  {
    { "emacs_so_code_1", 7, emacs_so_code_1 },
    { "emacs_so_code_2", 8, emacs_so_code_2 },
    { "emacs_so_code_3", 13, emacs_so_code_3 },
    { "emacs_so_code_4", 2, emacs_so_code_4 },
    { "emacs_so_code_5", 5, emacs_so_code_5 },
    { "emacs_so_code_6", 5, emacs_so_code_6 },
    { "emacs_so_code_7", 6, emacs_so_code_7 },
    { "emacs_so_code_8", 15, emacs_so_code_8 },
    { "emacs_so_code_9", 8, emacs_so_code_9 },
    { "emacs_so_code_10", 8, emacs_so_code_10 },
    { "emacs_so_code_11", 9, emacs_so_code_11 },
    { "emacs_so_code_12", 7, emacs_so_code_12 },
    { "emacs_so_code_13", 4, emacs_so_code_13 },
    { "emacs_so_code_14", 5, emacs_so_code_14 },
    { "emacs_so_code_15", 1, emacs_so_code_15 },
    { "emacs_so_code_16", 22, emacs_so_code_16 },
    { "emacs_so_code_17", 16, emacs_so_code_17 },
    { "emacs_so_code_18", 8, emacs_so_code_18 },
    { "emacs_so_code_19", 1, emacs_so_code_19 },
    { "emacs_so_code_20", 1, emacs_so_code_20 },
    { "emacs_so_code_21", 1, emacs_so_code_21 },
    { "emacs_so_code_22", 3, emacs_so_code_22 },
    { "emacs_so_code_23", 1, emacs_so_code_23 },
    { "emacs_so_code_24", 1, emacs_so_code_24 }
  };

int
decl_emacs_so_64215321dbe48238 (void)
{
  DECLARE_SUBCODE_MULTIPLE (arr_decl_emacs_so_64215321dbe48238);
  return (0);
}

DECLARE_COMPILED_CODE ("emacs.so", 10, decl_emacs_so_64215321dbe48238, emacs_so_64215321dbe48238)

#endif /* !WANT_ONLY_DATA */

#ifndef WANT_ONLY_CODE

static const unsigned char prog_emacs_so_data_64215321dbe48238 [4004] =
  "\x62\x2e\xe5\x07\x1d\x0c\xb8\x0d\x1d\xb0\x81\x88\x1d\x08\x28\x0d"
  "\x28\x0d\x28\x0d\xb9\x28\x0d\x23\x22\x29\x22\x29\x22\x29\x22\x29"
  "\x22\x29\x22\x29\x22\x29\x21\x17\x2b\xba\x1d\xb0\x82\x88\x0d\xbb"
  "\x06\x28\x0d\xbc\x28\x0d\xbd\x28\x0d\xbe\x28\x0d\x28\x0d\xbf\x28"
  "\x0d\x28\x0d\x23\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29"
  "\x22\x29\x22\x29\x21\x17\x2b\x1c\x1d\xb0\x83\x88\x0c\x0c\x0c\x08"
  "\x88\x0c\x0c\x1d\xc1\x1c\x06\x07\x85\xc2\x1c\x02\xc1\x1c\x08\x28"
  "\x0d\x1c\x28\x0d\x23\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22"
  "\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x21"
  "\x17\x2b\x1c\x1d\xb0\x84\x88\x0d\x24\x28\x0d\x23\x22\x29\x22\x29"
  "\x21\x17\x2b\x1c\x1d\xb0\x85\x88\x08\x28\x0d\x28\x0d\x1c\x28\x0d"
  "\x23\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x21\x17\x2b\x1c\x1d"
  "\xb0\x86\x88\x0d\x1c\x24\x28\x0d\x1c\x28\x0d\x28\x0d\x1c\x28\x0d"
  "\x1c\x28\x0d\x23\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x21\x17"
  "\x2b\x1c\x1d\xb0\x02\x88\x0c\x0d\x1c\x24\x28\x1b\x28\x0d\x28\x0d"
  "\x1c\x28\x1b\x23\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29"
  "\x21\x17\x2b\x1c\x1d\xb0\x02\x88\x0c\x80\x83\xc1\x1c\x0c\x08\x28"
  "\x0d\x28\x0d\x28\x0d\x1c\x28\x0d\x28\x0d\x1c\x28\x0d\x28\x0d\x28"
  "\xb6\x28\x0d\x1c\x23\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22"
  "\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22"
  "\x29\x22\x29\x21\x17\x2b\x1c\x1d\xb0\x02\x88\x02\x1b\x0c\x0d\x1c"
  "\x24\x28\x0d\x28\x0d\x28\x1b\x28\x1b\x23\x22\x29\x22\x29\x22\x29"
  "\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x21\x17\x2b\x1c\x1d\xb0"
  "\x02\x88\x0d\x08\x0d\x0d\x1c\x08\x88\x07\x28\x0d\x28\x0d\x1c\x28"
  "\x0d\x28\x0d\x1c\x28\x0d\x28\x0d\x28\x1b\x28\x0d\x23\x22\x29\x22"
  "\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x21\x17\x2b"
  "\x1c\x1d\xb0\x02\x88\x1b\x08\x88\x1b\x06\x07\x85\x1b\x02\x1b\x08"
  "\x28\x0d\x28\x0d\x1c\x28\x0d\x1c\x28\x0d\x1c\x28\x0d\x1c\x23\x22"
  "\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22"
  "\x29\x21\x17\x2b\x1c\x1d\xb0\x02\x88\x28\x0d\x1c\x28\x1b\x28\x0d"
  "\x28\x1b\x28\x0d\x28\x1b\x28\x0d\x1c\x23\x22\x29\x22\x29\x22\x29"
  "\x22\x29\x22\x29\x22\x29\x22\x29\x21\x17\x2b\x1c\x1d\xb0\x02\x88"
  "\x1b\x08\x88\x0d\x24\x28\x1b\x28\x0d\x28\x0d\x1c\x23\x22\x29\x22"
  "\x29\x22\x29\x22\x29\x21\x17\x2b\x1c\x1d\xb0\x02\x88\x07\x0d\x1c"
  "\x25\x28\x1b\x28\x0d\x1c\x28\x1b\x28\x0d\x23\x22\x29\x22\x29\x22"
  "\x29\x22\x29\x22\x29\x21\x17\x2b\x1c\x1d\xb0\x02\x88\x28\x0d\x23"
  "\x22\x29\x21\x9f\x2b\x1c\x1d\xb0\x02\x88\x0c\x0c\x0c\x0c\x0c\x0c"
  "\x08\x88\x0d\x1c\x0d\x1c\x1b\x1b\x08\x07\x1b\x0d\x24\x28\x1b\x28"
  "\x1b\x28\x1b\x28\x1b\x28\x0d\x28\xb4\x28\xb6\x28\x1b\x23\x22\x29"
  "\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29"
  "\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29"
  "\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x21\x17\x2b\x1c\x1d\xb0"
  "\x02\x88\x0c\x0c\x0c\x0c\x0c\x88\xb3\x0c\x0c\x88\x1b\x0c\x0c\x88"
  "\x0c\x0c\x08\x88\x1b\x08\x1b\x24\x28\xb1\x28\x0d\x1c\x28\xb4\x28"
  "\xb6\x28\xb5\x28\x1b\x28\x1b\x28\x0d\x23\x22\x29\x22\x29\x22\x29"
  "\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29"
  "\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x21\x17\x2b\x1c\x1d\xb0"
  "\x02\x88\x0c\xb3\x0c\x08\x88\x08\x0c\x08\x88\x0c\x1b\x24\x28\xb1"
  "\x28\x1b\x28\xb4\x28\x0d\x1c\x23\x22\x29\x22\x29\x22\x29\x22\x29"
  "\x22\x29\x22\x29\x22\x29\x22\x29\x21\x17\x2b\x1c\x1d\xb0\x02\x88"
  "\x0c\x08\x88\x08\x0c\x08\x88\x0c\x22\x29\x21\x17\x2b\x1c\x1d\xb0"
  "\x02\x88\x22\x29\x21\x9c\x2b\x1c\x1d\xb0\x02\x88\x22\x29\x21\x9c"
  "\x2b\x1c\x1d\xb0\x02\x88\x1b\x25\x1b\x24\x22\x29\x22\x29\x22\x29"
  "\x21\x17\x2b\xbe\x1d\xb0\x02\x88\x22\x29\x21\x9c\x2b\xbd\x1d\xb0"
  "\x02\x88\x22\x29\x21\x9c\x2b\xbc\x17\xbb\x88\xb3\x1b\x08\x88\xc3"
  "\x1b\x0d\x0d\x0d\x0d\x0d\x0d\x0d\x0d\x0d\x0d\x0d\x0d\x0d\x1b\x0d"
  "\x1b\x1b\x1b\x1b\x1b\x17\xb4\x2a\xb5\x2a\xb6\x2a\x1b\x2a\x1b\x2a"
  "\x1b\x2a\x1b\x2a\x1b\x2a\x1b\x2a\x1b\x2a\x1b\x2a\x1b\x2a\x1b\x2a"
  "\x1b\x2a\x1b\x2a\x06\x1b\x2a\x1b\x2a\x06\x1b\x2a\x17\x1b\x1b\x1b"
  "\xb7\x1b\x1b\x1b\x9e\x1b\x2a\x0c\x1b\x2a\x1b\x2a\x1b\x2a\x1b\x2a"
  "\xb2\x2a\x9e\x1b\x28\x0d\x26\x28\x0d\x28\x0d\x28\x1b\x28\x1b\x28"
  "\x1b\x23\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29\x22\x29"
  "\x22\x29\x22\x29\x22\x29\x21\x17\x02\x51\x2f\x55\x73\x65\x72\x73"
  "\x2f\x68\x6f\x6e\x67\x71\x69\x61\x6e\x74\x61\x6e\x2f\x2e\x65\x6d"
  "\x61\x63\x73\x2e\x64\x2f\x65\x6c\x70\x61\x2f\x67\x65\x69\x73\x65"
  "\x72\x2d\x32\x30\x32\x30\x30\x32\x31\x33\x2e\x33\x35\x35\x2f\x73"
  "\x63\x68\x65\x6d\x65\x2f\x6d\x69\x74\x2f\x67\x65\x69\x73\x65\x72"
  "\x2f\x65\x6d\x61\x63\x73\x2e\x69\x6e\x66\x15\x23\x5b\x70\x75\x72"
  "\x69\x66\x69\x63\x61\x74\x69\x6f\x6e\x2d\x72\x6f\x6f\x74\x5d\x02"
  "\x02\x03\x19\x66\x6f\x72\x2d\x65\x61\x63\x68\x2d\x69\x6e\x74\x65"
  "\x72\x6e\x65\x64\x2d\x73\x79\x6d\x62\x6f\x6c\x03\x0c\x73\x79\x6d"
  "\x62\x6f\x6c\x2d\x6e\x61\x6d\x65\x04\x13\x65\x6e\x76\x69\x72\x6f"
  "\x6e\x6d\x65\x6e\x74\x2d\x62\x6f\x75\x6e\x64\x3f\x04\x12\x73\x74"
  "\x72\x69\x6e\x67\x2d\x70\x72\x65\x66\x69\x78\x2d\x63\x69\x3f\x05"
  "\x11\x10\x81\x87\x02\x10\x0e\x81\x85\x02\x0f\x0c\x81\x85\x02\x0e"
  "\x0a\x81\x85\x02\x0d\x08\x81\x85\x02\x0c\x06\x81\x87\x02\x0b\x04"
  "\x84\x06\x0f\x1d\x02\x07\x6e\x6f\x72\x6d\x61\x6c\x04\x1b\x65\x6e"
  "\x76\x69\x72\x6f\x6e\x6d\x65\x6e\x74\x2d\x72\x65\x66\x65\x72\x65"
  "\x6e\x63\x65\x2d\x74\x79\x70\x65\x03\x16\x77\x69\x74\x68\x2d\x6f"
  "\x75\x74\x70\x75\x74\x2d\x74\x6f\x2d\x73\x74\x72\x69\x6e\x67\x03"
  "\x0b\x70\x72\x6f\x63\x65\x64\x75\x72\x65\x3f\x04\x13\x65\x6e\x76"
  "\x69\x72\x6f\x6e\x6d\x65\x6e\x74\x2d\x6c\x6f\x6f\x6b\x75\x70\x03"
  "\x11\x72\x65\x61\x64\x2d\x66\x72\x6f\x6d\x2d\x73\x74\x72\x69\x6e"
  "\x67\x03\x0c\x73\x74\x72\x69\x6e\x67\x2d\x74\x72\x69\x6d\x03\x03"
  "\x70\x61\x08\x19\x12\x81\x83\x02\x18\x10\x81\x8d\x02\x17\x0e\x81"
  "\x8b\x02\x16\x0c\x81\x85\x02\x15\x0a\x81\x87\x02\x14\x08\x81\x89"
  "\x02\x13\x06\x81\x89\x02\x12\x04\x84\x06\x11\x25\x09\x02\x05\x61"
  "\x72\x67\x73\x09\x72\x65\x71\x75\x69\x72\x65\x64\x04\x6b\x65\x79"
  "\x09\x6f\x70\x74\x69\x6f\x6e\x61\x6c\x04\x2e\x2e\x2e\x04\x04\x63"
  "\x61\x72\x0a\x0d\x6f\x62\x6a\x65\x63\x74\x2d\x74\x79\x70\x65\x3f"
  "\x0b\x1e\x04\x63\x64\x72\x0c\x04\x11\x6f\x70\x65\x72\x61\x74\x6f"
  "\x72\x2d\x61\x72\x67\x6c\x69\x73\x74\x0d\x03\x08\x72\x65\x76\x65"
  "\x72\x73\x65\x03\x26\x1c\x81\x91\x02\x25\x1a\x81\x8f\x02\x24\x18"
  "\x81\x8f\x02\x23\x16\x81\x8f\x02\x22\x14\x81\x8f\x02\x21\x12\x81"
  "\x8f\x02\x20\x10\x81\x8f\x02\x1f\x0e\x81\x83\x02\x1e\x0c\x81\x8f"
  "\x02\x1d\x0a\x81\x8f\x02\x1c\x08\x81\x8d\x02\x1b\x06\x81\x85\x02"
  "\x1a\x04\x84\x06\x1b\x31\x0e\x02\x05\x72\x65\x61\x64\x02\x04\x17"
  "\x77\x69\x74\x68\x2d\x69\x6e\x70\x75\x74\x2d\x66\x72\x6f\x6d\x2d"
  "\x73\x74\x72\x69\x6e\x67\x02\x28\x06\x81\x85\x02\x27\x04\x83\x04"
  "\x05\x0d\x0f\x02\x03\x0e\x6e\x61\x6d\x65\x2d\x3e\x70\x61\x63\x6b"
  "\x61\x67\x65\x03\x11\x70\x61\x63\x6b\x61\x67\x65\x2f\x63\x68\x69"
  "\x6c\x64\x72\x65\x6e\x10\x04\x0b\x61\x70\x70\x65\x6e\x64\x2d\x6d"
  "\x61\x70\x04\x2d\x0c\x81\x85\x02\x2c\x0a\x81\x83\x02\x2b\x08\x83"
  "\x04\x2a\x06\x81\x81\x02\x29\x04\x82\x02\x0b\x16\x11\x02\x19\x61"
  "\x6e\x6f\x6e\x79\x6d\x6f\x75\x73\x2d\x70\x61\x63\x6b\x61\x67\x65"
  "\x2d\x70\x72\x65\x66\x69\x78\x12\x02\x03\x15\x65\x6e\x76\x69\x72"
  "\x6f\x6e\x6d\x65\x6e\x74\x2d\x3e\x70\x61\x63\x6b\x61\x67\x65\x13"
  "\x03\x0c\x6f\x62\x6a\x65\x63\x74\x2d\x68\x61\x73\x68\x03\x0d\x70"
  "\x61\x63\x6b\x61\x67\x65\x2f\x6e\x61\x6d\x65\x14\x03\x10\x77\x72"
  "\x69\x74\x65\x2d\x74\x6f\x2d\x73\x74\x72\x69\x6e\x67\x15\x04\x07"
  "\x73\x74\x72\x69\x6e\x67\x06\x32\x0c\x81\x85\x02\x31\x0a\x81\x85"
  "\x02\x30\x08\x81\x85\x02\x2f\x06\x81\x83\x02\x2e\x04\x83\x04\x0b"
  "\x1b\x16\x02\x08\x06\x7e\x73\x20\x3d\x3e\x0c\x67\x65\x69\x73\x65"
  "\x72\x2d\x72\x65\x70\x6c\x17\x02\x03\x13\x04\x11\x73\x65\x74\x2d"
  "\x72\x65\x70\x6c\x2f\x70\x72\x6f\x6d\x70\x74\x21\x05\x07\x66\x6f"
  "\x72\x6d\x61\x74\x13\x03\x14\x05\x38\x0e\x81\x89\x02\x37\x0c\x81"
  "\x87\x02\x36\x0a\x81\x87\x02\x35\x08\x81\x85\x02\x34\x06\x81\x83"
  "\x02\x33\x04\x83\x04\x0d\x1c\x18\x02\x09\x05\x2e\x69\x6e\x66\x0e"
  "\x73\x74\x72\x69\x6e\x67\x2d\x6c\x65\x6e\x67\x74\x68\x19\x04\x73"
  "\x63\x6d\x03\x08\x65\x6e\x74\x69\x74\x79\x3f\x03\x22\x63\x6f\x6d"
  "\x70\x69\x6c\x65\x64\x2d\x65\x6e\x74\x72\x79\x2f\x66\x69\x6c\x65"
  "\x6e\x61\x6d\x65\x2d\x61\x6e\x64\x2d\x69\x6e\x64\x65\x78\x05\x0a"
  "\x73\x75\x62\x73\x74\x72\x69\x6e\x67\x1a\x04\x0f\x73\x74\x72\x69"
  "\x6e\x67\x2d\x73\x75\x66\x66\x69\x78\x3f\x03\x13\x66\x69\x78\x2d"
  "\x6d\x69\x74\x2d\x73\x6f\x75\x72\x63\x65\x2d\x64\x69\x72\x1b\x03"
  "\x11\x65\x6e\x74\x69\x74\x79\x2d\x70\x72\x6f\x63\x65\x64\x75\x72"
  "\x65\x03\x14\x63\x6f\x6d\x70\x69\x6c\x65\x64\x2d\x70\x72\x6f\x63"
  "\x65\x64\x75\x72\x65\x3f\x03\x04\x0e\x73\x74\x72\x69\x6e\x67\x2d"
  "\x61\x70\x70\x65\x6e\x64\x1c\x0a\x47\x20\x81\x87\x02\x46\x1e\x84"
  "\x06\x45\x1c\x81\x89\x02\x44\x1a\x81\x87\x02\x43\x18\x84\x06\x42"
  "\x16\x81\x87\x02\x41\x14\x81\x85\x02\x40\x12\x81\x85\x02\x3f\x10"
  "\x81\x83\x02\x3e\x0e\x81\x83\x02\x3d\x0c\x81\x89\x02\x3c\x0a\x81"
  "\x89\x02\x3b\x08\x81\x85\x02\x3a\x06\x81\x85\x02\x39\x04\x83\x04"
  "\x1f\x3b\x1d\x02\x0a\x1c\x19\x1c\x2f\x75\x73\x72\x2f\x6c\x69\x62"
  "\x2f\x6d\x69\x74\x2d\x73\x63\x68\x65\x6d\x65\x2d\x78\x38\x36\x2d"
  "\x36\x34\x2f\x23\x67\x65\x69\x73\x65\x72\x3a\x6d\x69\x74\x2d\x73"
  "\x63\x68\x65\x6d\x65\x2d\x73\x6f\x75\x72\x63\x65\x2d\x64\x69\x72"
  "\x65\x63\x74\x6f\x72\x79\x19\x02\x04\x0f\x73\x74\x72\x69\x6e\x67"
  "\x2d\x70\x72\x65\x66\x69\x78\x3f\x03\x0d\x73\x74\x72\x69\x6e\x67"
  "\x2d\x6e\x75\x6c\x6c\x3f\x05\x1a\x04\x1c\x05\x4f\x12\x81\x85\x02"
  "\x4e\x10\x81\x87\x02\x4d\x0e\x81\x83\x02\x4c\x0c\x81\x83\x02\x4b"
  "\x0a\x81\x85\x02\x4a\x08\x81\x83\x02\x49\x06\x83\x04\x48\x04\x81"
  "\x83\x02\x11\x22\x1c\x02\x0b\x07\x72\x65\x73\x75\x6c\x74\x07\x6f"
  "\x75\x74\x70\x75\x74\x05\x75\x73\x65\x72\x1a\x02\x13\x6f\x70\x65"
  "\x6e\x2d\x6f\x75\x74\x70\x75\x74\x2d\x73\x74\x72\x69\x6e\x67\x04"
  "\x0d\x66\x69\x6e\x64\x2d\x70\x61\x63\x6b\x61\x67\x65\x1e\x04\x14"
  "\x77\x69\x74\x68\x2d\x6f\x75\x74\x70\x75\x74\x2d\x74\x6f\x2d\x70"
  "\x6f\x72\x74\x03\x14\x70\x61\x63\x6b\x61\x67\x65\x2f\x65\x6e\x76"
  "\x69\x72\x6f\x6e\x6d\x65\x6e\x74\x1f\x03\x12\x67\x65\x74\x2d\x6f"
  "\x75\x74\x70\x75\x74\x2d\x73\x74\x72\x69\x6e\x67\x04\x05\x65\x76"
  "\x61\x6c\x03\x15\x03\x06\x77\x72\x69\x74\x65\x09\x57\x12\x81\x8d"
  "\x02\x56\x10\x81\x8b\x02\x55\x0e\x81\x83\x02\x54\x0c\x81\x8b\x02"
  "\x53\x0a\x81\x8b\x02\x52\x08\x81\x89\x02\x51\x06\x81\x87\x02\x50"
  "\x04\xfd\x07\x11\x2a\x20\x02\x0c\x1a\x0c\x0b\x1e\x0a\x03\x06\x6c"
  "\x69\x73\x74\x3f\x03\x0f\x67\x65\x69\x73\x65\x72\x3a\x61\x75\x74"
  "\x6f\x64\x6f\x63\x0b\x04\x04\x6d\x61\x70\x21\x03\x0e\x2d\x3e\x65"
  "\x6e\x76\x69\x72\x6f\x6e\x6d\x65\x6e\x74\x22\x04\x18\x67\x65\x69"
  "\x73\x65\x72\x3a\x6f\x70\x65\x72\x61\x74\x6f\x72\x2d\x61\x72\x67"
  "\x6c\x69\x73\x74\x23\x06\x60\x14\x81\x85\x02\x5f\x12\x83\x04\x5e"
  "\x10\x81\x85\x02\x5d\x0e\x81\x85\x02\x5c\x0c\x81\x83\x02\x5b\x0a"
  "\x81\x87\x02\x5a\x08\x81\x85\x02\x59\x06\x81\x85\x02\x58\x04\xfe"
  "\x05\x13\x2a\x24\x02\x0d\x02\x0d\x61\x6c\x6c\x2d\x70\x61\x63\x6b"
  "\x61\x67\x65\x73\x25\x03\x15\x04\x07\x66\x69\x6c\x74\x65\x72\x04"
  "\x21\x04\x0b\x73\x75\x62\x73\x74\x72\x69\x6e\x67\x3f\x03\x1f\x03"
  "\x0d\x65\x6e\x76\x2d\x3e\x70\x73\x74\x72\x69\x6e\x67\x15\x08\x67"
  "\x10\x81\x83\x02\x66\x0e\x83\x04\x65\x0c\x81\x85\x02\x64\x0a\x81"
  "\x87\x02\x63\x08\x81\x85\x02\x62\x06\x81\x85\x02\x61\x04\xfe\x05"
  "\x0f\x21\x26\x02\x0e\x1a\x09\x73\x74\x72\x69\x6e\x67\x3c\x3f\x02"
  "\x03\x22\x04\x05\x73\x6f\x72\x74\x04\x10\x61\x6c\x6c\x2d\x63\x6f"
  "\x6d\x70\x6c\x65\x74\x69\x6f\x6e\x73\x22\x04\x6b\x0a\x81\x87\x02"
  "\x6a\x08\x81\x85\x02\x69\x06\x81\x85\x02\x68\x04\xfe\x05\x09\x16"
  "\x27\x02\x0f\x0b\x67\x65\x69\x73\x65\x72\x2d\x65\x6e\x76\x28\x02"
  "\x04\x1e\x03\x18\x73\x65\x74\x2d\x67\x65\x69\x73\x65\x72\x2d\x72"
  "\x65\x70\x6c\x2d\x70\x72\x6f\x6d\x70\x74\x21\x29\x03\x1f\x03\x03"
  "\x67\x65\x05\x70\x0c\x81\x85\x02\x6f\x0a\x81\x85\x02\x6e\x08\x81"
  "\x85\x02\x6d\x06\x81\x83\x02\x6c\x04\x83\x04\x0b\x1a\x2a\x02\x10"
  "\x03\x05\x6c\x6f\x61\x64\x02\x71\x04\x83\x04\x03\x2b\x02\x11\x0a"
  "\x73\x69\x67\x6e\x61\x74\x75\x72\x65\x05\x76\x61\x72\x73\x06\x70"
  "\x72\x6f\x63\x73\x07\x73\x79\x6e\x74\x61\x78\x08\x6d\x6f\x64\x75"
  "\x6c\x65\x73\x0a\x73\x69\x67\x6e\x61\x74\x75\x72\x65\x06\x6d\x61"
  "\x63\x72\x6f\x2c\x0b\x75\x6e\x61\x73\x73\x69\x67\x6e\x65\x64\x2d"
  "\x0c\x0a\x14\x05\x6c\x69\x73\x74\x03\x04\x1e\x03\x10\x03\x1f\x04"
  "\x21\x03\x15\x65\x6e\x76\x69\x72\x6f\x6e\x6d\x65\x6e\x74\x2d\x62"
  "\x69\x6e\x64\x69\x6e\x67\x73\x04\x03\x04\x23\x09\x87\x01\x2e\x81"
  "\x97\x02\x86\x01\x2c\x81\x97\x02\x85\x01\x2a\x81\x97\x02\x84\x01"
  "\x28\x81\x97\x02\x83\x01\x26\x81\x8f\x02\x82\x01\x24\x81\x97\x02"
  "\x81\x01\x22\x81\x95\x02\x80\x01\x20\x81\x95\x02\x7f\x1e\x81\x95"
  "\x02\x7e\x1c\x81\x95\x02\x7d\x1a\x81\x91\x02\x7c\x18\x81\x8f\x02"
  "\x7b\x16\x81\x93\x02\x7a\x14\x81\x8f\x02\x79\x12\x81\x93\x02\x78"
  "\x10\x81\x87\x02\x77\x0e\x81\x85\x02\x76\x0c\x81\x87\x02\x75\x0a"
  "\x81\x85\x02\x74\x08\x81\x83\x02\x73\x06\x81\x83\x02\x72\x04\x83"
  "\x04\x2d\x50\x21\x02\x12\x11\x50\x72\x6f\x63\x65\x64\x75\x72\x65"
  "\x3a\x7e\x25\x7e\x61\x7e\x25\x0d\x56\x61\x6c\x75\x65\x3a\x7e\x25"
  "\x7e\x61\x7e\x25\x0a\x64\x6f\x63\x73\x74\x72\x69\x6e\x67\x0a\x64"
  "\x6f\x63\x73\x74\x72\x69\x6e\x67\x11\x55\x6e\x6b\x6e\x6f\x77\x6e"
  "\x20\x74\x68\x69\x6e\x67\x2e\x2e\x2e\x0a\x64\x6f\x63\x73\x74\x72"
  "\x69\x6e\x67\x14\x56\x61\x6c\x75\x65\x3a\x20\x55\x6e\x61\x73\x73"
  "\x69\x67\x6e\x65\x64\x7e\x25\x2d\x0a\x64\x6f\x63\x73\x74\x72\x69"
  "\x6e\x67\x06\x4d\x61\x63\x72\x6f\x0a\x73\x69\x67\x6e\x61\x74\x75"
  "\x72\x65\x05\x61\x72\x67\x73\x2c\x28\x02\x04\x04\x18\x65\x6e\x76"
  "\x69\x72\x6f\x6e\x6d\x65\x6e\x74\x2d\x73\x61\x66\x65\x2d\x6c\x6f"
  "\x6f\x6b\x75\x70\x2d\x04\x03\x03\x04\x23\x05\x13\x03\x03\x70\x70"
  "\x09\x97\x01\x22\x81\x83\x02\x96\x01\x20\x81\x8b\x02\x95\x01\x1e"
  "\x81\x89\x02\x94\x01\x1c\x81\x83\x02\x93\x01\x1a\x81\x89\x02\x92"
  "\x01\x18\x81\x87\x02\x91\x01\x16\x81\x87\x02\x90\x01\x14\x81\x89"
  "\x02\x8f\x01\x12\x81\x87\x02\x8e\x01\x10\x81\x85\x02\x8d\x01\x0e"
  "\x81\x89\x02\x8c\x01\x0c\x81\x83\x02\x8b\x01\x0a\x81\x87\x02\x8a"
  "\x01\x08\x81\x83\x02\x89\x01\x06\x81\x87\x02\x88\x01\x04\x83\x04"
  "\x21\x43\x2c\x02\x13\x05\x66\x69\x6c\x65\x05\x66\x69\x6c\x65\x05"
  "\x6c\x69\x6e\x65\x05\x6e\x61\x6d\x65\x28\x02\x04\x04\x2d\x04\x03"
  "\x1f\x67\x65\x74\x2d\x73\x79\x6d\x62\x6f\x6c\x2d\x64\x65\x66\x69"
  "\x6e\x69\x74\x69\x6f\x6e\x2d\x6c\x6f\x63\x61\x74\x69\x6f\x6e\x2d"
  "\x05\x9f\x01\x12\x81\x87\x02\x9e\x01\x10\x81\x85\x02\x9d\x01\x0e"
  "\x81\x89\x02\x9c\x01\x0c\x81\x83\x02\x9b\x01\x0a\x81\x87\x02\x9a"
  "\x01\x08\x81\x83\x02\x99\x01\x06\x81\x87\x02\x98\x01\x04\x83\x04"
  "\x11\x25\x14\x02\x14\x05\x66\x69\x6c\x65\x05\x6c\x69\x6e\x65\x05"
  "\x6e\x61\x6d\x65\xa0\x01\x04\x83\x04\x03\x0a\x10\x02\x15\xa1\x01"
  "\x04\x82\x02\x03\x0c\x02\x16\xa2\x01\x04\x82\x02\x03\x0a\x02\x17"
  "\x19\x02\x19\x02\xa5\x01\x08\x81\x85\x02\xa4\x01\x06\x81\x83\x02"
  "\xa3\x01\x04\x83\x04\x07\x0e\x02\x18\xa6\x01\x04\x83\x04\x03\x02"
  "\x19\xa7\x01\x04\x83\x04\x03\x19\x1a\x11\x6c\x6f\x63\x61\x6c\x2d"
  "\x61\x73\x73\x69\x67\x6e\x6d\x65\x6e\x74\x17\x0f\x67\x65\x69\x73"
  "\x65\x72\x3a\x63\x61\x6c\x6c\x65\x65\x73\x0f\x67\x65\x69\x73\x65"
  "\x72\x3a\x63\x61\x6c\x6c\x65\x72\x73\x27\x67\x65\x69\x73\x65\x72"
  "\x3a\x73\x65\x74\x2d\x6d\x69\x74\x2d\x73\x63\x68\x65\x6d\x65\x2d"
  "\x73\x6f\x75\x72\x63\x65\x2d\x64\x69\x72\x65\x63\x74\x6f\x72\x79"
  "\x11\x67\x65\x69\x73\x65\x72\x3a\x6e\x6f\x2d\x76\x61\x6c\x75\x65"
  "\x73\x0f\x67\x65\x69\x73\x65\x72\x3a\x6e\x65\x77\x6c\x69\x6e\x65"
  "\x17\x67\x65\x69\x73\x65\x72\x3a\x6d\x6f\x64\x75\x6c\x65\x2d\x6c"
  "\x6f\x63\x61\x74\x69\x6f\x6e\x17\x67\x65\x69\x73\x65\x72\x3a\x73"
  "\x79\x6d\x62\x6f\x6c\x2d\x6c\x6f\x63\x61\x74\x69\x6f\x6e\x1c\x67"
  "\x65\x69\x73\x65\x72\x3a\x73\x79\x6d\x62\x6f\x6c\x2d\x64\x6f\x63"
  "\x75\x6d\x65\x6e\x74\x61\x74\x69\x6f\x6e\x16\x67\x65\x69\x73\x65"
  "\x72\x3a\x6d\x6f\x64\x75\x6c\x65\x2d\x65\x78\x70\x6f\x72\x74\x73"
  "\x11\x67\x65\x69\x73\x65\x72\x3a\x6c\x6f\x61\x64\x2d\x66\x69\x6c"
  "\x65\x0a\x67\x65\x69\x73\x65\x72\x3a\x67\x65\x13\x67\x65\x69\x73"
  "\x65\x72\x3a\x63\x6f\x6d\x70\x6c\x65\x74\x69\x6f\x6e\x73\x1a\x67"
  "\x65\x69\x73\x65\x72\x3a\x6d\x6f\x64\x75\x6c\x65\x2d\x63\x6f\x6d"
  "\x70\x6c\x65\x74\x69\x6f\x6e\x73\x0b\x0c\x67\x65\x69\x73\x65\x72"
  "\x3a\x65\x76\x61\x6c\x19\x1b\x2d\x28\x29\x15\x04\x04\x04\x0a\x04"
  "\x0c\x04\x10\x04\x14\x04\x2c\x04\x21\x04\x2b\x04\x2a\x04\x27\x04"
  "\x26\x04\x24\x04\x20\x04\x1c\x06\x1d\x04\x18\x04\x15\x15\x12\x25"
  "\x23\x0d\x22\x16\x04\x0d\x65\x6e\x76\x69\x72\x6f\x6e\x6d\x65\x6e"
  "\x74\x2d\x11\x04\x0f\x04\x0e\x04\x09\x04\x04\x13\x05\x10\x64\x65"
  "\x66\x69\x6e\x65\x2d\x6d\x75\x6c\x74\x69\x70\x6c\x65\x02\x03\x0c"
  "\x6c\x6f\x61\x64\x2d\x6f\x70\x74\x69\x6f\x6e\x02\x0d\x6e\x65\x61"
  "\x72\x65\x73\x74\x2d\x72\x65\x70\x6c\x03\x1e\x03\x29\x03\x1f\x06"
  "\x0a\x16\x80\x80\x04\x09\x14\x81\x81\x02\x08\x12\x81\x81\x02\x07"
  "\x10\x81\x85\x02\x06\x0e\x81\x83\x02\x05\x0c\x81\x83\x02\x04\x0a"
  "\x81\x85\x02\x03\x08\x81\x83\x02\x02\x06\x81\x83\x02\x01\x04\x81"
  "\x83\x02\x15\x30";

SCHEME_OBJECT *
emacs_so_data_64215321dbe48238 (entry_count_t dispatch_base)
{
  SCHEME_OBJECT ccb;
  SCHEME_OBJECT * current_block;
  DECLARE_VARIABLES_FOR_DATA ();

  ccb = (unstackify (((unsigned char *) (& (prog_emacs_so_data_64215321dbe48238 [0]))), (sizeof (prog_emacs_so_data_64215321dbe48238)), dispatch_base));
  current_block = (OBJECT_ADDRESS (ccb));
  return (& (current_block [LABEL_19]));
}

DECLARE_COMPILED_DATA_NS ("emacs.so", emacs_so_data_64215321dbe48238)

#endif /* !WANT_ONLY_CODE */

DECLARE_DYNAMIC_INITIALIZATION ("emacs.so", "e7cdb266c1ac7b84")
