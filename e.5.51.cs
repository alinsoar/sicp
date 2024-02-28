/* -*- c -*- Mono C# compiler version 6.10.0.0 */

/* This micro-scheme implements the minimal amount of Scheme using
 * techniques similar to how MIT Scheme (and MacLisp and Lisp 1.5
 * before it) does it. */

using System;
using System.Collections.Generic;
using System.Text;

namespace MicroScheme
{
    using Sexp = System.Object;
    using SexpList = List<object>;
    using Scode = ValueTuple<Scheme_Object_Type, object>;
    using Formals = List<string>;
    using static Scheme_Object_Type;
    using static System.Console;

    enum SCODE_DEFINE { VALUE = 1, NAME = 0, };
    enum SCODE_LAMBDA { FORMALS = 1, EXPRESSION = 2};
    enum SCODE_CONDITIONAL {PREDICATE = 0, CONSEQUENT = 1, ALTERNATIVE = 2};
    enum SCODE_DYNAMIC_BIND {NAME = 0, VALUE = 1, EXPRESSION = 2};
    enum Scheme_Object_Type
    {
        SYMBOL,
        INTEGER,
        APPLICATION,
        EMPTY_LIST,
        DEFINE,
        LAMBDA,
        CONDITIONAL,
        KEYWORD,
        NO_KEYWORD,
        DYNAMIC_BIND,
        EOS,
        UNBOUND,
    };

    delegate object Primitive0();
    delegate object Primitive1(object arg0);
    delegate object Primitive2(object arg0, object arg1);

    class Interpreter
    {
        static private SexpList ARGLIST;
        static private Continuation REG_CONTINUE;
        static private Environment REG_ENVIRONMENT;
        static private object REG_EXPRESSION;
        static private object REG_VALUE;
        static private readonly Stack<object> STACK = new Stack<object>();
        static readonly Stack<object> STACKPDL = new Stack<object>();

        private enum Continuation
        {
            Accumulate_Final_Subexpression,
            Accumulate_Subexpression,
            Define_Continue,
            Return,
            If_Decide,
            Dynamic_Bind_Do_Expression,
            Dynamic_Bind_Restore_Binding,
        };

        static private object
        Interpret()
        {
        DO_EXPRESSION:
            {
                var (type, data) = ((Scode)REG_EXPRESSION);

                /* Out.Write("eval expression: "); */
                /* Out.WriteLine("\t\t"+type); */

                switch(type)
                {
                case SYMBOL:
                    REG_VALUE = REG_ENVIRONMENT.Lookup((string) data);
                    goto CONTINUE;
                case INTEGER:
                    REG_VALUE = (int)data;
                    goto CONTINUE;
                case EMPTY_LIST:
                    REG_VALUE = (SexpList)data;
                    goto CONTINUE;
                case DEFINE:
                    REG_EXPRESSION = (List<Scode>)data;
                    push_Nth_then(Continuation.Define_Continue, (int)SCODE_DEFINE.VALUE);
                    goto DO_EXPRESSION;
                case LAMBDA:
                    REG_VALUE = new Closure(REG_ENVIRONMENT, (SexpList)data);
                    goto CONTINUE;
                case CONDITIONAL:
                    REG_EXPRESSION = (List<Scode>)data;
                    push_Nth_then(Continuation.If_Decide, (int)SCODE_CONDITIONAL.PREDICATE);
                    goto DO_EXPRESSION;
                case APPLICATION:
                    REG_EXPRESSION = (List<Scode>)data;
                    ARGLIST = new SexpList ();
                    goto EVLIS;
                case DYNAMIC_BIND:
                    REG_EXPRESSION = (List<Scode>)data;
                    push_Nth_then(Continuation.Dynamic_Bind_Do_Expression, (int)SCODE_DYNAMIC_BIND.VALUE);
                    goto DO_EXPRESSION;
                default:
                    throw new Scheme_Error("never happens/unknown type:" + type);
                }
            }

        APPLY:
            {
                Sexp op = ARGLIST[0];

                switch(op.GetType().Name)
                {
                case nameof(Closure):
                    Closure closure = (Closure)op;
                    REG_ENVIRONMENT = new Environment (closure, ARGLIST);
                    REG_EXPRESSION = closure.Lambda[(int)SCODE_LAMBDA.EXPRESSION];
                    goto DO_EXPRESSION;
                case "Primitive0":
                    REG_VALUE = ((Primitive2)op).DynamicInvoke ();
                    break;
                case "Primitive1":
                    REG_VALUE = ((Primitive2)op).DynamicInvoke (ARGLIST[1]);
                    break;
                case "Primitive2":
                    REG_VALUE = ((Primitive2)op).DynamicInvoke (ARGLIST[1], ARGLIST[2]);
                    break;
                default:
                    throw new Scheme_Error(
                        "Application of non-procedure object: " + ARGLIST[0]);
                }
            }

        CONTINUE:
            {
                /* Out.WriteLine (">"+REG_CONTINUE+">"); */

                switch (REG_CONTINUE)
                {
                case Continuation.Accumulate_Final_Subexpression:
                {
                    REG_CONTINUE = (Continuation) STACK.Pop ();
                    ARGLIST = (SexpList) STACK.Pop ();
                    ARGLIST.Add (REG_VALUE);
                    goto APPLY;
                }
                case Continuation.Accumulate_Subexpression:
                {
                    restore_cont();
                    ARGLIST = (SexpList) STACK.Pop ();
                    ARGLIST.Add (REG_VALUE);
                    REG_EXPRESSION = ((List<Scode>) REG_EXPRESSION)
                    . GetRange (1, ((List<Scode>) REG_EXPRESSION).Count - 1);
                    goto EVLIS;
                }
                case Continuation.Define_Continue:
                {
                    restore_cont();
                    string name = (string)(((List<Scode>)REG_EXPRESSION)
                                           [(int)SCODE_DEFINE.NAME]).Item2;
                    REG_ENVIRONMENT.Define(name, REG_VALUE);
                    REG_VALUE = "* <"+name+">";
                    goto CONTINUE;
                }
                case Continuation.Return:
                {
                    return REG_VALUE;
                }
                case Continuation.If_Decide:
                {
                    restore_cont();
                    REG_EXPRESSION = ((List<Scode>) REG_EXPRESSION)
                    [(int)((REG_VALUE is bool valBool && valBool == false) ?
                           SCODE_CONDITIONAL.ALTERNATIVE :
                           SCODE_CONDITIONAL.CONSEQUENT)];
                    goto DO_EXPRESSION;
                }
                case Continuation.Dynamic_Bind_Do_Expression:
                {
                    restore_cont();
                    string name = (string)(((List<Scode>)REG_EXPRESSION) [(int)SCODE_DYNAMIC_BIND.NAME]).Item2;
                    Environment env = REG_ENVIRONMENT;
                    STACKPDL.Push(env.Lookup(name));
                    env.Set(name, REG_VALUE);
                    push_Nth_then(Continuation.Dynamic_Bind_Restore_Binding, (int)SCODE_DYNAMIC_BIND.EXPRESSION);
                    goto DO_EXPRESSION;
                }
                case Continuation.Dynamic_Bind_Restore_Binding:
                {
                    restore_cont();
                    string name = (string)(((List<Scode>)REG_EXPRESSION) [(int)SCODE_DYNAMIC_BIND.NAME]).Item2;
                    ((Environment)REG_ENVIRONMENT).Set(name, STACKPDL.Pop());
                    goto CONTINUE;
                }
                default:
                    throw new Scheme_Error (
                        "Unrecognized value for continuation: " + REG_CONTINUE);
                }
            }

        EVLIS:
            {
                if (((List<Scode>) REG_EXPRESSION).Count == 1)
                {
                    STACK.Push (ARGLIST);
                    STACK.Push (REG_CONTINUE);
                    REG_CONTINUE = Continuation.Accumulate_Final_Subexpression;
                    REG_EXPRESSION = ((List<Scode>) REG_EXPRESSION)[0];
                    goto DO_EXPRESSION;
                }
                else
                {
                    STACK.Push (ARGLIST);
                    save_cont();
                    REG_CONTINUE = Continuation.Accumulate_Subexpression;
                    REG_EXPRESSION = ((List<Scode>) REG_EXPRESSION)[0];
                    goto DO_EXPRESSION;
                }
            }
        }

        static private void
        save_cont()
        {
            STACK.Push (REG_CONTINUE);
            STACK.Push (REG_ENVIRONMENT);
            STACK.Push (REG_EXPRESSION);
        }

        static private void
        restore_cont()
        {
            REG_EXPRESSION = STACK.Pop ();
            REG_ENVIRONMENT = (Environment) STACK.Pop ();
            REG_CONTINUE = (Continuation)STACK.Pop ();
        }

        static private void
        push_Nth_then(Continuation return_code, int n)
        {
            /* sometimes REG_EXPRESSION is Scheme_Object_Type, at other times C# internal types */
            save_cont();
            REG_CONTINUE = return_code;
            REG_EXPRESSION = ((List<Scode>)REG_EXPRESSION)[n];
        }

        static public object
        EVAL(Environment env, object expr)
        {
            REG_CONTINUE = Continuation.Return;
            REG_ENVIRONMENT = env;
            REG_EXPRESSION = expr;
            return Interpret();
        }
    }

    class MicroScheme
    {
        static public SexpList
        make_lambda(string name, Formals variables, Sexp expression)
        {
            return new SexpList () { (object)name, (object)variables, (object)expression };
        }

        static object division(object left, object right)
        {
            int a, b;
            a = (int) left;
            b = (int) right;
            if (0==b)
            {
                return "division by 0";
            }
            return (object) (a/b);
        }
        
        /* SYSTEM ENVIRONMENT */
        static readonly Formals primitive_names = new Formals ()
        {
            "lessp",
            "add",
            "sub",
            "mod",
            "div",
        };

        static readonly SexpList primitive_values = new SexpList ()
        {
            new Primitive2 ((object left, object right) => (int) left < (int) right),
            new Primitive2 ((object left, object right) => (int) left + (int) right),
            new Primitive2 ((object left, object right) => (int) left - (int) right),
            new Primitive2 ((object left, object right) => (int) left % (int) right),
            new Primitive2(division),
            
/*             new Primitive2 ( => ), */
        };

        static readonly SexpList primitive_lambda =
        make_lambda("@system", primitive_names, null );

        static readonly Environment system_environment =
        new Environment (new Closure(null, primitive_lambda), primitive_values);

        /* GLOBAL ENVIRONMENT */
        static readonly SexpList global_lambda =
        make_lambda("@global", new Formals () { }, null);

        static readonly Environment global_environment =
        new Environment(new Closure(system_environment, global_lambda), new SexpList () { });

        static void Main(string[] args)
        {
            Formals free_variables = new Formals () { };

            try
            {
                while (true)
                {
                    free_variables.Clear();
                    Sexp expr = Parser.get_next_sexp(free_variables, global_environment);
                    if (expr == null) break;
                    free_variables.ForEach( f=>{ global_environment.Define(f, (UNBOUND, f)); });
                    /* free_variables.ForEach(a => Out.WriteLine ("f. "+a)); */
                    /* Out.WriteLine(global_environment); */
                    Out.WriteLine (Interpreter.EVAL(global_environment, expr).ToString ());
                }
            }
            catch (Scheme_Error e)
            {Out.WriteLine("SCHEME INTERPRETER ERROR "+e.Message);}
            catch (Exception e)
            {Out.WriteLine("ERROR "+e);}
        }
    }

    class Closure
    {
        public Environment Environment { get; set; }
        public SexpList Lambda { get; set; }

        public Closure (Environment environment, SexpList lambda)
        {
            Environment = environment;
            Lambda = lambda;
        }

        public int IndexOf (string variable)
        {
            return ((Formals) Lambda[(int)SCODE_LAMBDA.FORMALS]).IndexOf (variable);
        }
    }

    class Environment
    {
        Closure closure;
        readonly SexpList values;

        public Environment(Closure closure, SexpList values)
        {
            this.closure = closure;
            this.values = values;
        }

        internal void Define (string name, object value)
        {
            SexpList oldLambda = (SexpList) closure.Lambda;
            Formals oldParamlist = (Formals) oldLambda[(int)SCODE_LAMBDA.FORMALS];
            Formals newParamlist = new Formals ();
            newParamlist.AddRange (oldParamlist);
            newParamlist.Add (name);

            /* mutate the current closure by adding NAME to the
             * closure's formals and the current values by adding
             * VALUE to the current environment frame */
            closure = new Closure (
                closure.Environment,
                MicroScheme.make_lambda(
                    (string)oldLambda[0],
                    newParamlist,
                    (Sexp)oldLambda[(int)SCODE_LAMBDA.EXPRESSION] ));
            values.Add (value);
        }

        internal object Lookup(string name)
        {
            int index = closure.IndexOf(name);
            return index == -1
                ? closure.Environment == null
                    ? throw new Scheme_Error("Variable not found: " + name)
                    : closure.Environment.Lookup(name)
                : values[index];
        }

        internal bool Contains(string name)
        {
            return closure.IndexOf(name) == -1
                ? closure.Environment == null
                    ? false
                    : closure.Environment.Contains(name)
                : true;
        }

        internal void Set(string name, object value)
        {
            int index = closure.IndexOf(name);
            if (index == -1)
                if (closure.Environment == null)
                    throw new Scheme_Error("Variable not found: " + name);
                else
                    closure.Environment.Set(name, value);
            else
                values[index]=value;
        }

        public override string ToString()
        {
            SexpList lam = (SexpList) closure.Lambda;
            Formals parameters = (Formals) lam[(int)SCODE_LAMBDA.FORMALS];
            string next = (closure.Environment == null)?"*":(""+closure.Environment);
            string checklen = (values.Count == parameters.Count)? "":" !! ";

            if (parameters.Count == 0)
                return ("{<empty> "
                        + "[" + values.Count + " values]"
                        + checklen
                        + "} => "
                        + next);
            return ("{"
                    +string.Join(", ", parameters)
                    +" ["
                    +values.Count+" values]"
                    +checklen
                    +"} => "
                    +next);
        }
    }

    class Scheme_Error: Exception
    {
        public Scheme_Error(string message): base(message) {}
    }

    class Parser
    {
        static Dictionary<string, Scheme_Object_Type>
        special_forms = new Dictionary<string, Scheme_Object_Type>()
        {
            {"if", CONDITIONAL},
            {"define", DEFINE},
            {"named-lambda", LAMBDA},
            {"with-dynamic-binding", DYNAMIC_BIND},
        };

        static private Scheme_Object_Type
        keyword_dispatch(string x)
        {
            try { return special_forms[x];}
            catch (KeyNotFoundException) { return NO_KEYWORD; }
        }

        /* attach tags to s-expressions such that to uniquely identify the syntax */
        static private Scode
        objectify(object sexp)
        {
            /* Out.WriteLine("sexp:"+sexp); */
            if (sexp is SexpList exprList)
            {
                if (exprList.Count == 0)
                    return (EMPTY_LIST, (object)exprList);
                else
                {
                    object head = exprList[0];
                    if (head is string maybe_keyword)
                    {
                        Scheme_Object_Type special = keyword_dispatch(maybe_keyword);
                        if (special != NO_KEYWORD)
                        {
                            switch(special)
                            {
                            case DEFINE:
                                List<Scode> def = exprList.ConvertAll(objectify);
                                return (special, new List<Scode>(){def[1],def[2]});
                            case LAMBDA:
                                /* scode of lambda keeps the same structure as the symbolic expression of lambda */
                                Formals p = (Formals) (((SexpList)exprList[1]).ConvertAll(w=>(string) w));
                                exprList[1] = p;
                                exprList[0] = p[0]; /* name of the named lambda */
                                exprList[2] = objectify(exprList[2]);
                                return (special, (object)exprList);
                            case CONDITIONAL:
                                List<Scode> cond = exprList.ConvertAll(objectify);
                                return (special, new List<Scode>(){cond[1],cond[2],cond[3]});
                            case DYNAMIC_BIND:
                                SexpList binding = (SexpList)exprList[1];
                                Scode
                                var = objectify(binding[0]),
                                value = objectify(binding[1]),
                                expression = objectify(exprList[2]);
                                return (DYNAMIC_BIND, new List<Scode>() {var, value, expression});
                            default:
                                throw new Scheme_Error("unknown special form "+special);
                            }
                        }
                    }
                    return (APPLICATION, (object)exprList.ConvertAll(objectify));
                }
            }
            else if (sexp is int x)
                return (INTEGER, (object)x);
            else if (sexp is string name)
                return (SYMBOL, (object)name);
            else if (sexp == null)
                return (EOS, (object)0);
            else
                throw new Scheme_Error("unknown expression "+sexp);
        }

        /* detect the list of free variables */
        static private void
        scan_code(Scode sexp, Formals free_variables, Formals bound_variables, Environment env)
        {
            var (type, data) = sexp;

            /* Out.WriteLine("scan: "+type); */
            switch(type)
            {
            case DEFINE:
            {
                List<Scode> def = (List<Scode>) data;
                string name = (string)((def[(int)SCODE_DEFINE.NAME]).Item2);
                Scode value = (Scode)def[(int)SCODE_DEFINE.VALUE];
                bool bound = bound_variables.Contains(name) || env.Contains(name);
                if (!bound) bound_variables.Add(name);
                scan_code(value, free_variables, bound_variables, env);
                break;
            }
            case LAMBDA:
            {
                SexpList lambda = (SexpList)data;
                Formals vars = (Formals)lambda[(int)SCODE_LAMBDA.FORMALS];
                Scode expr = (Scode)lambda[(int)SCODE_LAMBDA.EXPRESSION];
                int index = bound_variables.Count;
                bound_variables.AddRange(vars);
                scan_code(expr, free_variables, bound_variables, env);
                bound_variables.RemoveRange(index, vars.Count);
                break;
            }
            case CONDITIONAL:
            {
                List<Scode> cond = (List<Scode>)data;
                Scode p = (Scode)cond[(int)SCODE_CONDITIONAL.PREDICATE];
                Scode c = (Scode)cond[(int)SCODE_CONDITIONAL.CONSEQUENT];
                Scode a = (Scode)cond[(int)SCODE_CONDITIONAL.ALTERNATIVE];
                scan_code(p, free_variables, bound_variables, env);
                scan_code(c, free_variables, bound_variables, env);
                scan_code(a, free_variables, bound_variables, env);
                break;
            }
            case APPLICATION:
            {
                List<Scode> e = (List<Scode>) data;
                e.ForEach(a => scan_code(a, free_variables, bound_variables, env));
                break;
            }
            case SYMBOL:
            {
                string sym_name = (string) data;
                bool bound = bound_variables.Contains(sym_name) || env.Contains(sym_name);
                if (!bound && !free_variables.Contains(sym_name))
                    free_variables.Add(sym_name);
                break;
            }
            case INTEGER:
            case EMPTY_LIST:
                break;
            case DYNAMIC_BIND:
                List<Scode> db = (List<Scode>)data;
                Scode v0 = (Scode)db[(int)SCODE_DYNAMIC_BIND.VALUE];
                Scode e0 = (Scode)db[(int)SCODE_DYNAMIC_BIND.EXPRESSION];
                scan_code(v0, free_variables, bound_variables, env);
                scan_code(e0, free_variables, bound_variables, env);
                break;
            default:
                throw new Scheme_Error("scan code/unknown type:" + type);
            }
        }

        static private object
        Read ()
        {
            DiscardWhitespace ();
            if (In.Peek () == -1) return null;
            else if (In.Peek ().Equals ('(')) {
                In.Read ();
                return ReadList ();
            } else return In.Peek ().Equals (')')
                   ? throw new Scheme_Error ("Unexpected end of list in Read.")
                   : ReadToken ();
        }

        static private void
        DiscardWhitespace ()
        {
            while (In.Peek() != -1 && char.IsWhiteSpace ((char) In.Peek())) {
                In.Read ();
            }
        }

        static private SexpList
        ReadList ()
        {
            SexpList answer = new SexpList () { };

            while (true) {
                DiscardWhitespace ();
                if (In.Peek () == -1)
                    throw new Scheme_Error ("End of file while reading list.");

                if (((char) In.Peek ()).Equals (')')) {
                    In.Read ();
                    return answer;
                }
                answer.Add (Read ());
            }
        }

        static private object
        ReadToken ()
        {
            StringBuilder stringBuffer = new StringBuilder ();
            while (char.IsLetterOrDigit((char)In.Peek())
                   || ((char)In.Peek()).Equals('-'))
                stringBuffer.Append ((char) In.Read ());
            try {
                return int.Parse (stringBuffer.ToString ());
            } catch (FormatException) {
                return stringBuffer.ToString ();
            }
        }

        static public object
        get_next_sexp(Formals free_variables, Environment env)
        {
            object sexp = Read();
            var (type, expr) = (Scode) objectify(sexp);
            if (EOS == type) return null;
            /* Out.WriteLine(env); */
            scan_code((type, expr), free_variables, new Formals(), env);
            return (type, expr);
        }
    }
}


