// @flow
/*
 *      logo.h          logo header file                dvb
 *
 *	Copyright (C) 1993 by the Regents of the University of California
 *
 *      This program is free software; you can redistribute it and/or modify
 *      it under the terms of the GNU General Public License as published by
 *      the Free Software Foundation; either version 2 of the License, or
 *      (at your option) any later version.
 *  
 *      This program is distributed in the hope that it will be useful,
 *      but WITHOUT ANY WARRANTY; without even the implied warranty of
 *      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *      GNU General Public License for more details.
 *  
 *      You should have received a copy of the GNU General Public License
 *      along with this program; if not, write to the Free Software
 *      Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 */


//JS #define ecma	/* for European extended character set using parity bit */

type mode_type = {'wrapmode', 'fencemode', 'windowmode'};

const WORDSIZE = 8 * 32;
const NIL = NodePtr(0);
const UNBOUND = Unbound;
const UNDEFINED = Unbound;
const END_OF_LIST = NodePtr(2);
const HASH_LEN = 1021;	/* a prime number */

const MAX_PHYS_LINE = 5000;
const MAX_NUMBER = 200;	/* max number of digits in a float */
const HIST_MAX = 50;	/* number of remembered instruction lines */

const STOP_PRIORITY = 0;
const OUTPUT_PRIORITY = 1;
const MAYBE_PRIORITY = 2;
const TAIL_PRIORITY = 2;	/* largest tailcall priority */
const MACRO_PRIORITY = 3;
const PREFIX_PRIORITY = 4;

type NODETYPES = {
    flags: Number,
    flags2: Number
};
const MakeNodeTypes = (flags:Number, flags2:?Number):NODETYPES => {
    if (!flags2) flags2 = 0;
    return {flags, flags2};
};
const CombineNodeTypes = (...flags:Array<NODETYPES>):NODETYPES => {
    let nt:NODETYPES = MakeNodeTypes(0);
    for (let n of flags) {
        nt.flags |= n.flags;
        nt.flags2 |= n.flags2;
    }
    return nt;
};

const NT_METHOD = MakeNodeTypes(0, 1);
const NT_OBJ = MakeNodeTypes(0x40000000);
const NT_LOCAL = MakeNodeTypes(0x20000000);
const NT_STACK = MakeNodeTypes(0x10000000);
const NT_CONT = MakeNodeTypes(0x4000000);
const NT_INFIX = MakeNodeTypes(0x2000000);
const NT_LINE		 = MakeNodeTypes(0x1000000);
const NT_TAILFORM = MakeNodeTypes(0x400000);
const NT_MACRO = MakeNodeTypes(0x200000);
const NT_TREE = MakeNodeTypes(0x100000);
const NT_EMPTY = MakeNodeTypes(0x40000);
const NT_AGGR = MakeNodeTypes(0x20000);
const NT_LIST = MakeNodeTypes(0x10000);
const NT_RUNP = MakeNodeTypes(0x04000);
const NT_ARRAY = MakeNodeTypes(0x2000);
const NT_WORD = MakeNodeTypes(0x1000);
const NT_NUMBER = MakeNodeTypes(0x400);
const NT_FLOAT = MakeNodeTypes(0x200);
const NT_PRIM = MakeNodeTypes(0x100);
const NT_VBAR = MakeNodeTypes(0x40);
const NT_STRING = MakeNodeTypes(0x20);
const NT_BACKSL = MakeNodeTypes(0x10);
const NT_PUNCT = MakeNodeTypes(0x4);
const NT_COLON = MakeNodeTypes(0x2);
const NT_CASEOBJ = MakeNodeTypes(0x1);

const PNIL = CombineNodeTypes(NT_EMPTY,NT_AGGR,NT_LIST);
const PUNBOUND = CombineNodeTypes();
const CONS = CombineNodeTypes(NT_AGGR,NT_LIST);
const STRING = CombineNodeTypes(NT_WORD,NT_STRING);
const INT = CombineNodeTypes(NT_WORD,NT_NUMBER);
const FLOATT= CombineNodeTypes(NT_WORD,NT_NUMBER,NT_FLOAT);
const PRIM		= CombineNodeTypes(NT_PRIM);
const MACRO= CombineNodeTypes(NT_PRIM,NT_MACRO);
const TAILFORM= CombineNodeTypes(NT_PRIM,NT_TAILFORM);
const CASEOBJ= CombineNodeTypes(NT_WORD,NT_CASEOBJ);
const INFIX= CombineNodeTypes(NT_PRIM,NT_INFIX);
const TREE= CombineNodeTypes(NT_AGGR,NT_LIST,NT_TREE);
const RUN_PARSE= CombineNodeTypes(NT_AGGR,NT_LIST,NT_RUNP);
const QUOTE= CombineNodeTypes(NT_WORD,NT_PUNCT);
const COLON= CombineNodeTypes(NT_WORD,NT_PUNCT,NT_COLON);
const BACKSLASH_STRING= CombineNodeTypes(NT_WORD,NT_STRING,NT_BACKSL);
const VBAR_STRING= CombineNodeTypes(NT_WORD,NT_STRING,NT_BACKSL,NT_VBAR);
const ARRAY= CombineNodeTypes(NT_AGGR,NT_ARRAY);
const LINE= CombineNodeTypes(NT_LINE,NT_LIST,NT_AGGR);
const CONT= CombineNodeTypes(NT_CONT,NT_LIST);
const STACK= CombineNodeTypes(NT_STACK,NT_LIST);
const NTFREE= MakeNodeTypes(0x4fffffff,0x4fffffff);
const LOCALSAVE	= CombineNodeTypes(CONS,NT_LOCAL);
const OBJECT      = CombineNodeTypes(NT_OBJ);
const METHOD	    = CombineNodeTypes(NT_AGGR,NT_LIST,NT_METHOD);

const aggregate = (nd:NODEPTR):Boolean => flagSet(nodetype(nd), NT_AGGR);
const is_cont = (nd:NODEPTR):Boolean => flagEqual(nodetype(nd), CONT);
const is_list = (nd:NODEPTR):Boolean => flagSet(nodetype(nd), NT_LIST);
const is_tree = (nd:NODEPTR):Boolean =>	flagSet(nodetype(nd), NT_TREE);
const is_string = (nd:NODEPTR):Boolean => flagSet(nodetype(nd), NT_STRING);
const is_number = (nd:NODEPTR):Boolean => flagSet(nodetype(nd), NT_NUMBER);
const is_prim = (nd:NODEPTR):Boolean =>	flagSet(nodetype(nd), NT_PRIM);
const is_word = (nd:NODEPTR):Boolean => flagSet(nodetype(nd), NT_WORD);
const runparsed = (nd:NODEPTR):Boolean => flagSet	(nodetype(nd), NT_RUNP);
const backslashed = (nd:NODEPTR):Boolean => flagSet	(nodetype(nd), NT_BACKSL);
const is_tailform = (nd:NODEPTR):Boolean =>	flagEqual(nodetype(nd), TAILFORM);
const is_object = (nd:NODEPTR):Boolean =>   flagEqual(nodetype(nd), NT_OBJ);
const is_method = (nd:NODEPTR):Boolean =>   flagEqual(nodetype(nd), NT_METHOD);

typedef enum { FATAL, OUT_OF_MEM, STACK_OVERFLOW, TURTLE_OUT_OF_BOUNDS,
		BAD_DATA_UNREC, DIDNT_OUTPUT, NOT_ENOUGH, BAD_DATA, TOO_MUCH,
		DK_WHAT, PAREN_MISMATCH, NO_VALUE, UNEXPECTED_PAREN, DK_HOW,
		NO_CATCH_TAG, ALREADY_DEFINED, STOP_ERROR, ALREADY_DRIBBLING,
		FILE_ERROR, IF_WARNING, SHADOW_WARN, USER_ERR, IS_PRIM,
		NOT_INSIDE, DK_HOW_UNREC, NO_TEST, UNEXPECTED_BRACKET,
		UNEXPECTED_BRACE, BAD_GRAPH_INIT, ERR_MACRO,
		DK_WHAT_UP, AT_TOPLEVEL, APPLY_BAD_DATA, DEEPEND,
		OUT_OF_MEM_UNREC, USER_ERR_MESSAGE, DEEPEND_NONAME,
		BAD_DEFAULT, RUNRES_STOP, MISSING_SPACE,
		CANT_OPEN_ERROR, ALREADY_OPEN_ERROR, NOT_OPEN_ERROR,
		RUNNABLE_ARG,
// I removed conditional inclusion of LOCAL_AND_OBJ because
// it the messages text file does not have the ability to 
// conditionally include the text for the message, resulting
// in a shift by 1 when compiled with OBJECTS
		LOCAL_AND_OBJ,
    /* below this point aren't actually error codes, just messages */
		THANK_YOU, NICE_DAY, NOSHELL_MAC, TYPE_EXIT, ERROR_IN,
		ERRACT_LOOP, PAUS_ING, TRACE_STOPS, TRACE_OUTPUTS,
		NO_FILE, NO_FIONREAD, MEM_LOW, CANT_OPEN, ALREADY_OPEN,
		NOT_OPEN, TRACE_PPROP, WELCOME_TO, CANT_STOP, CANT_GC,
		EXIT_NOW, LOAD_DEF, TRACE_MAKE, EMPTY_PROC, POT_PLIST,
		NO_HELP, NO_HELPON, MORE_HELP,
		MAX_MESSAGE} ERR_TYPES;	    /* MAX_MESSAGE must be last */

#ifdef WIN32
#define BOOLEAN int
#else
typedef int BOOLEAN;
#endif

#define FALSE	0
#define TRUE	1

#define SPECIAL_COLORS 5
#define FILLED_COLOR_INDEX 0
#define FILLED_COLOR_OFFSET -5
#define BACKGROUND_COLOR_INDEX 1
#define BACKGROUND_COLOR_OFFSET -4
#define PEN_COLOR_INDEX 2
#define PEN_COLOR_OFFSET -3
#define TEXT_BG_COLOR_INDEX 3
#define TEXT_BG_COLOR_OFFSET -2
#define TEXT_FG_COLOR_INDEX 4
#define TEXT_FG_COLOR_OFFSET -1


#define even_p(x) !(x & 0x1)

#define FIXNUM          long
#define FLONUM          double
#define MAXLOGOINT	0x7fffffff
#define SAFEINT		0x00003fff  /* safe to multiply w/o overflow */

struct string_block {
    unsigned FIXNUM str_refcnt;
    char str_str[1];	    /* This array will be of variable length really */
};

#define getstrrefcnt(sh)        ((sh)->str_refcnt)
#define setstrrefcnt(sh, v)     ((sh)->str_refcnt = (v))
#define incstrrefcnt(sh)        (((sh)->str_refcnt)++)
#define decstrrefcnt(sh)        (--((sh)->str_refcnt))

typedef struct logo_node {
    NODETYPES node_type;
    int my_gen; /* Nodes's Generation */ /*GC*/
    int gen_age; /* How many times to GC at this generation */
    long int mark_gc;	/* when marked */
    struct logo_node *next; /* Link together nodes of the same age */ /*GC*/
    struct logo_node *oldyoung_next;
    union {
	struct {
	    struct logo_node *ncar;
	    struct logo_node *ncdr;
	    struct logo_node *nobj;         /* used only for oblist etc */
	} ncons;
	struct {
	    char *nstring_ptr;
	    struct string_block *nstring_head;
	    FIXNUM nstring_len;
	} nstring;
	struct {
	    struct logo_node * (*nprim_fun) ();
	    short npriority;
	    short nmin_args;
	    short ndef_args;
	    short nmax_args;
	} nprim;
	FIXNUM nint;
	FLONUM nfloat;
	struct {
	    FIXNUM narray_dim;
	    FIXNUM narray_origin;
	    struct logo_node **narray_data;
	} narray;
#ifdef OBJECTS
	struct {
	    struct logo_node *nvars;
	    struct logo_node *nprocs;
	    struct logo_node *nparents;
	} nobject;
	struct {
	    struct logo_node *nparent;
	    struct logo_node *nprocname;
	} nmethod;
#endif
    } nunion;
} NODE;

#define settype(node, type)     ((node)->node_type = (type))

#define n_car                   nunion.ncons.ncar
#define n_cdr                   nunion.ncons.ncdr
#define n_obj                   nunion.ncons.nobj
#define getobject(node)         ((node)->n_obj)
#define car(node)               ((node)->n_car)
#define cdr(node)               ((node)->n_cdr)
#define caar(node)              ((node)->n_car->n_car)
#define cadr(node)              ((node)->n_cdr->n_car)
#define cdar(node)              ((node)->n_car->n_cdr)
#define cddr(node)              ((node)->n_cdr->n_cdr)

#define n_str                   nunion.nstring.nstring_ptr
#define n_len                   nunion.nstring.nstring_len
#define n_head                  nunion.nstring.nstring_head
#define getstrptr(node)         ((node)->n_str)
#define getstrlen(node)         ((node)->n_len)
#define getstrhead(node)        ((node)->n_head)
#define setstrptr(node,ptr)     ((node)->n_str = (ptr))
#define setstrlen(node,len)     ((node)->n_len = (len))
#define setstrhead(node,ptr)    ((node)->n_head = (ptr))

#define n_int                   nunion.nint
#define getint(node)            ((node)->n_int)
#define setint(node,num)        ((node)->n_int = (num))

#define n_float                 nunion.nfloat
#define getfloat(node)          ((node)->n_float)
#define setfloat(node,num)      ((node)->n_float = (num))

#define n_pfun                  nunion.nprim.nprim_fun
#define n_ppri                  nunion.nprim.npriority
#define n_pmin                  nunion.nprim.nmin_args
#define n_pdef                  nunion.nprim.ndef_args
#define n_pmax                  nunion.nprim.nmax_args
#define getprimfun(node)        ((node)->n_pfun)
#define setprimfun(node,fun)    ((node)->n_pfun = (fun))
#define getprimmin(node)        ((node)->n_pmin)
#define setprimmin(node,num)    ((node)->n_pmin = (num))
#define getprimmax(node)        ((node)->n_pmax)
#define setprimmax(node,num)    ((node)->n_pmax = (num))
#define getprimdflt(node)       ((node)->n_pdef)
#define setprimdflt(node,num)   ((node)->n_pdef = (num))
#define getprimpri(node)        ((node)->n_ppri)
#define setprimpri(node,num)    ((node)->n_ppri = (num))
/* Special value for pmin, means that it's
 *  OK if primitive name on line by itself even though defltargs=1 (ED, CO) */
#define OK_NO_ARG	01000

#define n_dim			nunion.narray.narray_dim
#define n_org			nunion.narray.narray_origin
#define n_array			nunion.narray.narray_data
#define getarrdim(node)		((node)->n_dim)
#define getarrorg(node)		((node)->n_org)
#define getarrptr(node)		((node)->n_array)
#define setarrdim(node,len)	((node)->n_dim = (len))
#define setarrorg(node,org)	((node)->n_org = (org))
#define setarrptr(node,ptr)	((node)->n_array = (ptr))

#ifdef OBJECTS
#define n_vars                    nunion.nobject.nvars          
#define n_procs                   nunion.nobject.nprocs         
#define n_parents                 nunion.nobject.nparents       
#define getvars(node)             ((node)->n_vars)              
#define getprocs(node)            ((node)->n_procs)             
#define getparents(node)          ((node)->n_parents)           
#define setvars(node, vars)       ((node)->n_vars = (vars))       
#define setprocs(node, procs)     ((node)->n_procs = (procs))     
#define setparents(node, parents) ((node)->n_parents = (parents)) 

#define getsymbol(frame)          car(frame)
#define getvalue(frame)           cadr(frame)

#define n_parent		nunion.nmethod.nparent
#define n_procname		nunion.nmethod.nprocname
#define getparent(node)		((node)->n_parent)
#define getprocname(node)	((node)->n_procname)
#endif	/* OBJECTS */

#ifdef ecma
#define clearparity(ch)		ecma_clear(ch)
#define setparity(ch)		ecma_set(ch)
#define getparity(ch)		ecma_get(ch)
#define ecma_begin		003	/* first char used for quoteds */
#else
#define clearparity(ch)         (ch & 0x7f)
#define setparity(ch)           (ch | 0x80)
#define getparity(ch)           (ch & 0x80)
#endif

typedef enum { RUN, STOP, OUTPUT, THROWING, MACRO_RETURN } CTRLTYPE;

struct segment {
	struct segment *next;
	FIXNUM size;
#ifdef mac
	struct logo_node nodes[1];
#else
#ifdef __RZTC__
	struct logo_node nodes[1];
#else
	struct logo_node nodes[0];
#endif
#endif
};

#define NOT_THROWING            (stopping_flag != THROWING)
#define RUNNING                 (stopping_flag == RUN)
#define STOPPING                (stopping_flag == STOP)

#define canonical__object(o)    car(o)
#define procnode__object(o)     cadr(o)
#define setprocnode__object(o,v) setcar(cdr(o), v)
#define valnode__object(o)      cadr(cdr(o))
#define setvalnode__object(o,v) setcar(cddr(o), v)
#define plist__object(o)        cadr(cddr(o))
#define setplist__object(o,v)	setcar(cdr(cddr(o)), v)
#define obflags__object(o)	car(cddr(cddr(o)))
#define caselistptr__object(o)  cddr(cddr(o))
#define caselist__object(o)     cdr(cddr(cddr(o)))

#define strnode__caseobj(co)    car(co)
#define object__caseobj(c)      cdr(c)
#define procnode__caseobj(c)    procnode__object(object__caseobj(c))
#define setprocnode__caseobj(c,v) setprocnode__object(object__caseobj(c),v)
#define valnode__caseobj(c)	valnode__object(object__caseobj(c))
#define setvalnode__caseobj(c,v) setvalnode__object(object__caseobj(c),v)
#define plist__caseobj(c)	plist__object(object__caseobj(c))
#define setplist__caseobj(c,v)	setplist__object(object__caseobj(c),v)
#define obflags__caseobj(c)	obflags__object(object__caseobj(c))

#define text__procnode(p)	car(p)
#define formals__procnode(p)    caar(p)
#define bodylist__procnode(p)   cdar(p)
#define bodywords__procnode(p)  cadr(p)
#define setbodywords__procnode(p,v) setcar(cdr(p),v)
#define minargs__procnode(p)    car(cddr(p))
#define dfltargs__procnode(p)   cadr(cddr(p))
#define maxargs__procnode(p)    car(cddr(cddr(p)))

#define unparsed__runparse(rn)  rn
#define parsed__runparse(rn)    getobject(rn)
#define node__quote(q)          car(q)
#define node__colon(c)          car(c)
#define valnode__colon(c)       valnode__caseobj(node__colon(c))

#define unparsed__tree(t)	t
#define treepair__tree(t)	(getobject(t))
#define settreepair__tree(t, v)	setobject(t, v)
#define generation__tree(t)	(car(treepair__tree(t)))
#define setgeneration__tree(t, g) setcar(treepair__tree(t), g)
#define tree__tree(t)		cdr(treepair__tree(t))
#define settree__tree(t, v)	settreepair__tree(t, cons(the_generation, v))

#define unparsed__line(l)	(getobject(l))
#define generation__line(l)	(generation__tree(unparsed__line(l)))
#define tree__line(l)		l

#define cont__cont(c)		(FIXNUM)car(c)
#define val__cont(c)		cdr(c)

/* Object flags.  Ones settable by users via bury_helper must come in threes
 * for proc, val, plist even if meaningless for some of those. */
#define PROC_BURIED	    01
#define VAL_BURIED	    02
#define PLIST_BURIED	    04
#define PROC_TRACED	    010
#define VAL_TRACED	    020
#define PLIST_TRACED	    040
#define PROC_STEPPED	    0100
#define VAL_STEPPED	    0200
#define PLIST_STEPPED	    0400
#define PROC_MACRO	    01000
#define PERMANENT	    02000
#define HAS_GLOBAL_VALUE    04000
#define IS_LOCAL_VALUE	    010000
#ifdef OBJECTS
#define MIXED_ARITY	    020000
#endif
#define PROC_SPECFORM	    040000

#define setflag__caseobj(c,f) ((obflags__caseobj(c))->n_int |= (f))
#define clearflag__caseobj(c,f) ((obflags__caseobj(c))->n_int &= ~(f))
#define flag__caseobj(c,f) (int)((obflags__caseobj(c))->n_int & (f))
#define flag__object(o,f) (int)((obflags__object(o))->n_int & (f))
#define setflag__object(c,f) ((obflags__object(c))->n_int |= (f))
#define clearflag__object(c,f) ((obflags__object(c))->n_int &= ~(f))
#define is_macro(c) (flag__caseobj(c, PROC_MACRO))

#define push(obj, stack)    stack = cons(obj, stack)
#define pop(stack)	    stack = cdr(stack)

/* evaluator labels, needed by macros in other files */
/* (Put the commonly used ones first.) */

#ifdef OBJECTS

#define do_list(x) \
    x(no_reset_args) x(eval_sequence_continue) \
    x(accumulate_arg) x(compound_apply_continue) \
    x(after_const_arg) x(begin_seq) x(begin_apply) \
    x(set_args_continue) x(macro_return) \
    x(repeat_continuation) x(repeat_followup) \
    x(runresult_continuation) x(runresult_followup) \
    x(catch_continuation) x(catch_followup) x(after_lambda) \
    x(after_maybeoutput) \
    x(withobject_continuation) x(withobject_followup) \
    x(goto_continuation) \
    x(begin_line) x(end_line) \
    x(all_done) \
    x(fall_off_want_output) x(op_want_stop) x(after_constant)

#else /* OBJECTS */

#define do_list(x) \
    x(no_reset_args) x(eval_sequence_continue) \
    x(accumulate_arg) x(compound_apply_continue) \
    x(after_const_arg) x(begin_seq) x(begin_apply) \
    x(set_args_continue) x(macro_return) \
    x(repeat_continuation) x(repeat_followup) \
    x(runresult_continuation) x(runresult_followup) \
    x(catch_continuation) x(catch_followup) x(after_lambda) \
    x(after_maybeoutput) \
    x(goto_continuation) \
    x(begin_line) x(end_line) \
    x(all_done) \
    x(fall_off_want_output) x(op_want_stop) x(after_constant)

#endif /* OBJECTS */

#define do_enum(x) x,

enum labels {
    do_list(do_enum)
    NUM_TOKENS
};

/* similarly, names that might be translated in Messages file */

#ifdef OBJECTS
#define do_trans(x) \
    x(true) x(false) x(end) x(output) x(stop) x(goto) x(tag) \
    x(if) x(ifelse) x(to) x(macro) x(toplevel) x(system) x(error) x(nothing) \
    x(textscreen) x(splitscreen) x(fullscreen) x(paint) x(erase) x(reverse) \
    x(wrap) x(fence) x(window) x(sum) x(difference) x(product) x(quotient) \
    x(equalp) x(lessp) x(greaterp) x(lessequalp) x(greaterequalp) \
    x(notequalp) \
    x(name) x(class) x(self) \
    x(licenseplate) x(initlist) x(exist)
#else
#define do_trans(x) \
    x(true) x(false) x(end) x(output) x(stop) x(goto) x(tag) \
    x(if) x(ifelse) x(to) x(macro) x(toplevel) x(system) x(error) x(nothing) \
    x(textscreen) x(splitscreen) x(fullscreen) x(paint) x(erase) x(reverse) \
    x(wrap) x(fence) x(window) x(sum) x(difference) x(product) x(quotient) \
    x(equalp) x(lessp) x(greaterp) x(lessequalp) x(greaterequalp) \
    x(notequalp)
#endif

#define wd_enum(x) Name_ ## x,

enum words {
    do_trans(wd_enum)
    NUM_WORDS
};

struct wdtrans {
    NODE *English;
    NODE *Alt;
};

/* evaluator val_status flags, used also in coms.c */

#define VALUE_OK 1	/* [instr instr instr exp] */
#define NO_VALUE_OK 2	/* [instr instr instr instr] */
#define OUTPUT_OK 4	/* [instr instr OUTPUT exp instr] */
#define STOP_OK 8	/* [instr instr STOP instr] */
#define OUTPUT_TAIL 16	/* not [repeat n [... output ...]] */
#define STOP_TAIL 32	/* not [repeat n [... stop ...]] */

/* evaluator registers that need saving around evals */
/* Node pointers must all come before other types.  proc must be first;
   val_status must be first non-node.  See end of init(). */

struct registers {
    NODE *r_proc;   /* the procedure definition */
    NODE *r_argl;   /* evaluated argument list */
    NODE *r_unev;   /* list of unevaluated expressions */
    NODE *r_fun;    /* current function name */
    NODE *r_ufun;   /* current user-defined function name */
    NODE *r_var;    /* frame pointer into var_stack */
    NODE *r_vsp;    /* temp ptr into var_stack */
    NODE *r_qm_list;	/* question mark list */
    NODE *r_formals;	/* list of formal parameters */
    NODE *r_last_ufun;	/* the function that called this one */
    NODE *r_this_line;	/* the current instruction line */
    NODE *r_last_line;	/* the line that called this one */
    NODE *r_current_unode;	/* a pair to identify this proc call */
    NODE *r_didnt_output_name;  /* name of the proc that didn't OP */
    NODE *r_didnt_get_output;	/* procedure wanting output from EVAL */
    FIXNUM r_val_status;    /* tells what EVAL_SEQUENCE should do: */
    FIXNUM r_tailcall;	    /* 0 in sequence, 1 for tail, -1 for arg */
    FIXNUM r_repcount;	    /* count for repeat */
    FIXNUM r_user_repcount;
    FIXNUM r_ift_iff_flag;
#ifdef OBJECTS
    NODE *r_usual_parent;
    NODE *r_usual_caller;
#endif

};

/* definitions for evaluator registers */

#ifdef WANT_EVAL_REGS
#define proc (regs.r_proc)
#define argl (regs.r_argl)
#define unev (regs.r_unev)
#define fun (regs.r_fun)
#define ufun (regs.r_ufun)
#define var (regs.r_var)
#define vsp (regs.r_vsp)
#define qm_list (regs.r_qm_list)
#define formals (regs.r_formals)
#define last_ufun (regs.r_last_ufun)
#define this_line (regs.r_this_line)
#define last_line (regs.r_last_line)
#define current_unode (regs.r_current_unode)
#define didnt_output_name (regs.r_didnt_output_name)
#define didnt_get_output (regs.r_didnt_get_output)
#define val_status (regs.r_val_status)
#define tailcall (regs.r_tailcall)
#define repcount (regs.r_repcount)
#define user_repcount (regs.r_user_repcount)
#define ift_iff_flag (regs.r_ift_iff_flag)

#ifdef OBJECTS
#define usual_parent (regs.r_usual_parent)
#define usual_caller (regs.r_usual_caller)
#endif

#endif

#endif /* _LOGO_H */
