



-- csv = 1*(record CRLF)   （1）
-- record = field *(COMMA field)   （2）
-- field = (escaped / non-escaped)   （3）
-- escaped = DQUOTE *(TEXTDATA / COMMA / CR / LF / 2DQUOTE) DQUOTE （4）
-- non-escaped = *TEXTDATA

-- TEXTDATA = %x20-21 / %x23-2B / %x2D-7E
-- COMMA = %x2C

-- CRLF = CR LF
-- CR = %x0D
-- LF = %x0A

-- DQUOTE = %x22


