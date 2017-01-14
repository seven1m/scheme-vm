use self :: RuleResult :: { Matched , Failed } ; fn escape_default ( s : & str ) -> String {
s . chars (  ) . flat_map ( | c | c . escape_default (  ) ) . collect (  ) }
fn char_range_at ( s : & str , pos : usize ) -> ( char , usize ) {
let c = & s [ pos .. ] . chars (  ) . next (  ) . unwrap (  ) ; let next_pos =
pos + c . len_utf8 (  ) ; ( * c , next_pos ) } # [ derive ( Clone ) ] enum
RuleResult < T > { Matched ( usize , T ) , Failed , } # [
derive ( PartialEq , Eq , Debug , Clone ) ] pub struct ParseError {
pub line : usize , pub column : usize , pub offset : usize , pub expected : ::
std :: collections :: HashSet < & 'static str > , } pub type ParseResult < T >
= Result < T , ParseError > ; impl :: std :: fmt :: Display for ParseError {
fn fmt ( & self , fmt : & mut :: std :: fmt :: Formatter ) -> :: std :: result
:: Result < (  ) , :: std :: fmt :: Error > {
try ! (
write ! ( fmt , "error at {}:{}: expected " , self . line , self . column ) )
; if self . expected . len (  ) == 0 { try ! ( write ! ( fmt , "EOF" ) ) ; }
else if self . expected . len (  ) == 1 {
try ! (
write ! (
fmt , "`{}`" , escape_default (
self . expected . iter (  ) . next (  ) . unwrap (  ) ) ) ) ; } else {
let mut iter = self . expected . iter (  ) ; try ! (
write ! (
fmt , "one of `{}`" , escape_default ( iter . next (  ) . unwrap (  ) ) ) ) ;
for elem in iter {
try ! ( write ! ( fmt , ", `{}`" , escape_default ( elem ) ) ) ; } } Ok ( (  )
) } } impl :: std :: error :: Error for ParseError {
fn description ( & self ) -> & str { "parse error" } } fn slice_eq (
input : & str , state : & mut ParseState , pos : usize , m : & 'static str )
-> RuleResult < (  ) > {
# ! [ inline ] # ! [ allow ( dead_code ) ] let l = m . len (  ) ; if input .
len (  ) >= pos + l && & input . as_bytes (  ) [ pos .. pos + l ] == m .
as_bytes (  ) { Matched ( pos + l , (  ) ) } else {
state . mark_failure ( pos , m ) } } fn slice_eq_case_insensitive (
input : & str , state : & mut ParseState , pos : usize , m : & 'static str )
-> RuleResult < (  ) > {
# ! [ inline ] # ! [ allow ( dead_code ) ] let mut used = 0usize ; let mut
input_iter = input [ pos .. ] . chars (  ) . flat_map (
| x | x . to_uppercase (  ) ) ; for m_char_upper in m . chars (  ) . flat_map
( | x | x . to_uppercase (  ) ) {
used += m_char_upper . len_utf8 (  ) ; let input_char_result = input_iter .
next (  ) ; if input_char_result . is_none (  ) || input_char_result . unwrap
(  ) != m_char_upper { return state . mark_failure ( pos , m ) ; } } Matched (
pos + used , (  ) ) } fn any_char (
input : & str , state : & mut ParseState , pos : usize ) -> RuleResult < (  )
> {
# ! [ inline ] # ! [ allow ( dead_code ) ] if input . len (  ) > pos {
let ( _ , next ) = char_range_at ( input , pos ) ; Matched ( next , (  ) ) }
else { state . mark_failure ( pos , "<character>" ) } } fn pos_to_line (
input : & str , pos : usize ) -> ( usize , usize ) {
let mut remaining = pos ; let mut lineno : usize = 1 ; for line in input .
lines (  ) {
let line_length = line . len (  ) + 1 ; if remaining < line_length {
return ( lineno , remaining + 1 ) ; } remaining -= line_length ; lineno += 1 ;
} return ( lineno , remaining + 1 ) ; } impl < 'input > ParseState < 'input >
{
fn mark_failure ( & mut self , pos : usize , expected : & 'static str ) ->
RuleResult < (  ) > {
if self . suppress_fail == 0 {
if pos > self . max_err_pos {
self . max_err_pos = pos ; self . expected . clear (  ) ; } if pos == self .
max_err_pos { self . expected . insert ( expected ) ; } } Failed } } struct ParseState < 'input > { max_err_pos : usize , suppress_fail : usize , expected : :: std :: collections :: HashSet < & 'static str > , _phantom : :: std :: marker :: PhantomData < & 'input ( ) > , } impl < 'input > ParseState < 'input > { fn new ( ) -> ParseState < 'input > { ParseState { max_err_pos : 0 , suppress_fail : 0 , expected : :: std :: collections :: HashSet :: new ( ) , _phantom : :: std :: marker :: PhantomData , } } } 

 fn parse_consonants < 'input > ( __input : & 'input str , __state : & mut ParseState < 'input > , __pos : usize ) -> RuleResult < () > { # ! [ allow ( non_snake_case , unused ) ] { let mut __repeat_pos = __pos ; let mut repeat_value = vec ! ( ) ; loop { let __pos = __repeat_pos ; let step_res = { let seq_res = if __input . len ( ) > __pos { let ( __ch , __next ) = char_range_at ( __input , __pos ) ; match __ch { 'a' ... 'z' => Matched ( __next , ( ) ) , _ => __state . mark_failure ( __pos , "[a-z]" ) , } } else { __state . mark_failure ( __pos , "[a-z]" ) } ; match seq_res { Matched ( __pos , _ ) => { { __state . suppress_fail += 1 ; let __assert_res = if __input . len ( ) > __pos { let ( __ch , __next ) = char_range_at ( __input , __pos ) ; match __ch { 'a' | 'e' | 'i' | 'o' | 'u' => Matched ( __next , ( ) ) , _ => __state . mark_failure ( __pos , "[aeiou]" ) , } } else { __state . mark_failure ( __pos , "[aeiou]" ) } ; __state . suppress_fail -= 1 ; match __assert_res { Failed => Matched ( __pos , ( ) ) , Matched ( .. ) => Failed , } } } Failed => Failed , } } ; match step_res { Matched ( newpos , value ) => { __repeat_pos = newpos ; repeat_value . push ( value ) ; } , Failed => { break ; } } } if repeat_value . len ( ) >= 1 { Matched ( __repeat_pos , ( ) ) } else { Failed } } } 

 pub fn consonants < 'input > ( input : & 'input str ) -> ParseResult < () > { # ! [ allow ( non_snake_case , unused ) ] let mut state = ParseState :: new ( ) ; match parse_consonants ( input , & mut state , 0 ) { Matched ( pos , value ) => { if pos == input . len ( ) { return Ok ( value ) } } _ => { } } let ( line , col ) = pos_to_line ( input , state . max_err_pos ) ; Err ( ParseError { line : line , column : col , offset : state . max_err_pos , expected : state . expected , } ) }