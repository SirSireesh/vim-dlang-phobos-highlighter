" Vim syntax file
" Language: D Additions
" Maintainer: Sireesh Kodali
" Last Change: 2017 10 11
" Version: 0.1
" Changelog:
"   0.1 - initial version.
"
" Additional Vim syntax highlighting for D
"
" This file contains additional syntax highlighting that I use for D
" development in Vim. Compared to the standard syntax highlighting for D it
" adds highlighting of (user defined) functions and the containers and types
" in the standard library / boost.
"
" Based on:
"   http://stackoverflow.com/q/736701
"   http://www.vim.org/scripts/script.php?script_id=4293
"   http://www.vim.org/scripts/script.php?script_id=2224
"   http://www.vim.org/scripts/script.php?script_id=1640
"   http://www.vim.org/scripts/script.php?script_id=3064


" -----------------------------------------------------------------------------
"  Highlight Class and Function names.
"
" Based on the discussion in: http://stackoverflow.com/q/736701
" -----------------------------------------------------------------------------

" Functions
syn match   dCustomParen    "(" contains=dParen "contains=cCppParen
syn match   dCustomFunc     "\w\+\s*(\@="
hi def link dCustomFunc  Function

" Class and namespace scope
if exists('g:d_class_scope_highlight') && g:d_class_scope_highlight
	syn match   dCustomScope    "."
	syn match   dCustomClass    "\w\+\s*."
				\ contains=dCustomScope
	hi def link dCustomClass Function
endif

" Alternative syntax that is used in:
"  http://www.vim.org/scripts/script.php?script_id=3064
"syn match dUserFunction "\<\h\w*\>\(\s\|\n\)*("me=e-1 contains=dType,dDelimiter,dDefine
"hi def link dCustomFunc  Function

" Cluster for all the stdlib functions defined below
syn cluster dSTLgroup     contains=dSTLfunction,dSTLfunctional,dSTLconstant,dSTLnamespace,dSTLtype,dSTLexception,,dSTLenum

" -----------------------------------------------------------------------------
"  Standard library types and functions.
"
" Mainly based on the excellent STL Syntax vim script by
" Mizuchi <ytj000@gmail.com>
"   http://www.vim.org/scripts/script.php?script_id=4293
" which in turn is based on the scripts
"   http://www.vim.org/scripts/script.php?script_id=2224
"   http://www.vim.org/scripts/script.php?script_id=1640
" -----------------------------------------------------------------------------

syntax keyword dSTLnamespace std
syntax keyword dSTLnamespace etc
syntax keyword dSTLnamespace core

"std.algorithm
syntax keyword dSTLfunction all
syntax keyword dSTLfunction any
syntax keyword dSTLfunction balancedParens
syntax keyword dSTLfunction boyerMooreFinder
syntax keyword dSTLfunction canFind
syntax keyword dSTLfunction commonPrefix
syntax keyword dSTLfunction count
syntax keyword dSTLfunction countUntil
syntax keyword dSTLfunction endsWith
syntax keyword dSTLfunction find
syntax keyword dSTLfunction findAdjacent
syntax keyword dSTLfunction findAmong
syntax keyword dSTLfunction findSkip
syntax keyword dSTLfunction findSplit
syntax keyword dSTLfunction findSplitAfter
syntax keyword dSTLfunction findSplitBefore
syntax keyword dSTLfunction minCount
syntax keyword dSTLfunction maxCount
syntax keyword dSTLfunction minElement
syntax keyword dSTLfunction maxElement
syntax keyword dSTLfunction minIndex
syntax keyword dSTLfunction maxIndex
syntax keyword dSTLfunction minPos
syntax keyword dSTLfunction maxPos
syntax keyword dSTLfunction skipOver
syntax keyword dSTLfunction startsWith
syntax keyword dSTLfunction until
syntax keyword dSTLfunction among
syntax keyword dSTLfunction castSwitch
syntax keyword dSTLfunction clamp
syntax keyword dSTLfunction cmp
syntax keyword dSTLfunction either
syntax keyword dSTLfunction equal
syntax keyword dSTLfunction isPermutation
syntax keyword dSTLfunction isSameLength
syntax keyword dSTLfunction levenshteinDistance
syntax keyword dSTLfunction levenshteinDistanceAndPath
syntax keyword dSTLfunction max
syntax keyword dSTLfunction min
syntax keyword dSTLfunction mismatch
syntax keyword dSTLfunction predSwitch
syntax keyword dSTLfunction cache
syntax keyword dSTLfunction cacheBidirectional
syntax keyword dSTLfunction chunkBy
syntax keyword dSTLfunction cumulativeFold
syntax keyword dSTLfunction each
syntax keyword dSTLfunction filter
syntax keyword dSTLfunction filterBidirectional
syntax keyword dSTLfunction fold
syntax keyword dSTLfunction group
syntax keyword dSTLfunction joiner
syntax keyword dSTLfunction map
syntax keyword dSTLfunction permutations
syntax keyword dSTLfunction reduce
syntax keyword dSTLfunction splitter
syntax keyword dSTLfunction sum
syntax keyword dSTLfunction uniq
syntax keyword dSTLfunction completeSort
syntax keyword dSTLfunction isPartitioned
syntax keyword dSTLfunction isSorted
syntax keyword dSTLfunction isStrictlyMonotonic
syntax keyword dSTLfunction ordered
syntax keyword dSTLfunction strictlyOrdered
syntax keyword dSTLfunction makeIndex
syntax keyword dSTLfunction merge
syntax keyword dSTLfunction multiSort
syntax keyword dSTLfunction nextEvenPermutation
syntax keyword dSTLfunction nextPermutation
syntax keyword dSTLfunction partialSort
syntax keyword dSTLfunction partition
syntax keyword dSTLfunction partition3
syntax keyword dSTLfunction schwartzSort
syntax keyword dSTLfunction sort
syntax keyword dSTLfunction topN
syntax keyword dSTLfunction topNCopy
syntax keyword dSTLfunction topNIndex
syntax keyword dSTLfunction cartesianProduct
syntax keyword dSTLfunction largestPartialIntersection
syntax keyword dSTLfunction largestPartialIntersectionWeighted
syntax keyword dSTLfunction nWayUnion
syntax keyword dSTLfunction setDifference
syntax keyword dSTLfunction setIntersection
syntax keyword dSTLfunction setSymmetricDifference
syntax keyword dSTLfunction bringToFront
syntax keyword dSTLfunction copy
syntax keyword dSTLfunction fill
syntax keyword dSTLfunction initializeAll
syntax keyword dSTLfunction move
syntax keyword dSTLfunction moveAll
syntax keyword dSTLfunction moveSome
syntax keyword dSTLfunction moveEmplace
syntax keyword dSTLfunction moveEmplaceAll
syntax keyword dSTLfunction moveEmplaceSome
syntax keyword dSTLfunction remove
syntax keyword dSTLfunction reverse
syntax keyword dSTLfunction strip
syntax keyword dSTLfunction stripLeft
syntax keyword dSTLfunction stripRight
syntax keyword dSTLfunction swap
syntax keyword dSTLfunction swapRanges
syntax keyword dSTLfunction uninitializedFill
"std.array
syntax keyword dSTLfunction array
syntax keyword dSTLfunction appender
syntax keyword dSTLfunction assocArray
syntax keyword dSTLfunction byPair
syntax keyword dSTLfunction insertInPlace
syntax keyword dSTLfunction join
syntax keyword dSTLfunction minimallyInitializedArray
syntax keyword dSTLfunction replace
syntax keyword dSTLfunction replaceFirst
syntax keyword dSTLfunction replaceInPlace
syntax keyword dSTLfunction replaceInto
syntax keyword dSTLfunction replaceLast
syntax keyword dSTLfunction replaceSlice
syntax keyword dSTLfunction replicate
syntax keyword dSTLfunction clear
syntax keyword dSTLfunction shrinkTo
syntax keyword dSTLfunction sameHead
syntax keyword dSTLfunction sameTail
syntax keyword dSTLfunction split
syntax keyword dSTLfunction uninitializedArray
"std.ascii
syntax keyword dSTLconstant digits
syntax keyword dSTLconstant fullHexDigits
syntax keyword dSTLconstant hexDigits
syntax keyword dSTLconstant letters
syntax keyword dSTLconstant lowercase
syntax keyword dSTLconstant lowerHexDigits
syntax keyword dSTLconstant newline
syntax keyword dSTLconstant octalDigits
syntax keyword dSTLconstant uppercase
syntax keyword dSTLconstant whitespace
syntax keyword dSTLfunction isAlpha
syntax keyword dSTLfunction isAlphaNum
syntax keyword dSTLfunction isASCII
syntax keyword dSTLfunction isControl
syntax keyword dSTLfunction isDigit
syntax keyword dSTLfunction isGraphical
syntax keyword dSTLfunction isHexDigit
syntax keyword dSTLfunction isOctalDigit
syntax keyword dSTLfunction isPrintable
syntax keyword dSTLfunction isPunctuation
syntax keyword dSTLfunction isUpper
syntax keyword dSTLfunction isWhite
syntax keyword dSTLfunction toLower
syntax keyword dSTLfunction toUpper
"std.base64
syntax keyword dSTLtype Encoder
syntax keyword dSTLtype Decoder
syntax keyword dSTLenum NoPadding
syntax keyword dSTLfunction encodeLength
syntax keyword dSTLfunction encode
syntax keyword dSTLfunction decodeLength
syntax keyword dSTLfunction decode
syntax keyword dSTLfunction encoder
syntax keyword dSTLfunction decoder
"std.bigint
syntax keyword dSTLtype BigInt
syntax keyword dSTLfunction toLong
syntax keyword dSTLfunction toInt
syntax keyword dSTLfunction uintLength
syntax keyword dSTLfunction ulongLength
syntax keyword dSTLfunction toDecimalString
syntax keyword dSTLfunction toHex
syntax keyword dSTLfunction absUnsign
"std.bitmanip
syntax keyword dSTLtype FloatRep
syntax keyword dSTLtype DoubleRep
syntax keyword dSTLtype BitArray
syntax keyword dSTLfunction bitfields
syntax keyword dSTLfunction bitsSet
syntax keyword dSTLfunction bigEndianToNative
syntax keyword dSTLfunction littleEndianToNative
syntax keyword dSTLfunction nativeToBigEndian
syntax keyword dSTLfunction nativeToLittleEndian
syntax keyword dSTLfunction swapEndian
syntax keyword dSTLfunction peek
syntax keyword dSTLfunction read
syntax keyword dSTLfunction taggedClassRef
syntax keyword dSTLfunction taggedPointer
"std.containers
syntax keyword dSTLtype Array
syntax keyword dSTLtype BinaryHeap
syntax keyword dSTLtype DList
syntax keyword dSTLtype RedBlackTree
syntax keyword dSTLtype SList
syntax keyword dSTLtype RefAppender
"std.complex
syntax keyword dSTLtype Complex
syntax keyword dSTLfunction arg
syntax keyword dSTLfunction complex
syntax keyword dSTLfunction conj
syntax keyword dSTLfunction fromPolar
syntax keyword dSTLfunction sqAbs
"std.concurrency
syntax keyword dSTLtype Tid
syntax keyword dSTLtype ThreadInfo
syntax keyword dSTLtype ThreadScheduler
syntax keyword dSTLtype FiberScheduler
syntax keyword dSTLtype Generator
syntax keyword dSTLenum OnCrowding
syntax keyword dSTLfunction start
syntax keyword dSTLfunction newCondition
syntax keyword dSTLfunction thisInfo
syntax keyword dSTLfunction cleanup
syntax keyword dSTLfunction initOnce
syntax keyword dSTLfunction locate
syntax keyword dSTLfunction ownerTid
syntax keyword dSTLfunction prioritySend
syntax keyword dSTLfunction receive
syntax keyword dSTLfunction receiveOnly
syntax keyword dSTLfunction receiveTimeout
syntax keyword dSTLfunction register
syntax keyword dSTLfunction Scheduler
syntax keyword dSTLfunction scheduler
syntax keyword dSTLfunction send
syntax keyword dSTLfunction setMaxMailboxSize
syntax keyword dSTLfunction spawn
syntax keyword dSTLfunction spawnLinked
syntax keyword dSTLfunction thisTid
syntax keyword dSTLfunction unregister
syntax keyword dSTLfunction yield
"std.compiler
syntax keyword dSTLconstant name
syntax keyword dSTLconstant vendor
syntax keyword dSTLconstant version_major
syntax keyword dSTLconstant version_minor
syntax keyword dSTLconstant D_major
syntax keyword dSTLenum Vendor
"std.conv
syntax keyword dSTLfunction castFrom
syntax keyword dSTLfunction emplace
syntax keyword dSTLfunction parse
syntax keyword dSTLfunction to
syntax keyword dSTLfunction toChars
syntax keyword dSTLfunction text
syntax keyword dSTLfunction wtext
syntax keyword dSTLfunction dtext
syntax keyword dSTLfunction hexString
syntax keyword dSTLfunction octal
syntax keyword dSTLfunction roundTo
syntax keyword dSTLfunction signed
syntax keyword dSTLfunction unsigned
"std.csv
syntax keyword dSTLenum Malformed
syntax keyword dSTLfunction csvNextToken
syntax keyword dSTLfunction csvReader
"std.datetime
syntax keyword dSTLtype SysTime
syntax keyword dSTLtype Date
syntax keyword dSTLtype TimeOfDay
syntax keyword dSTLtype DateTime
syntax keyword dSTLtype FormatSpec
syntax keyword dSTLenum LetterCase
syntax keyword dSTLenum Month
syntax keyword dSTLenum DayOfWeek
syntax keyword dSTLenum Direction
syntax keyword dSTLconstant timeStrings
syntax keyword dSTLfunction currTime
syntax keyword dSTLfunction currStdTime
syntax keyword dSTLfunction year
syntax keyword dSTLfunction yearBC
syntax keyword dSTLfunction month
syntax keyword dSTLfunction day
syntax keyword dSTLfunction hour
syntax keyword dSTLfunction minute
syntax keyword dSTLfunction second
syntax keyword dSTLfunction fracSecs
syntax keyword dSTLfunction stdTime
syntax keyword dSTLfunction timezone
syntax keyword dSTLfunction dstInEffect
syntax keyword dSTLfunction utcOffset
syntax keyword dSTLfunction toLocalTime
syntax keyword dSTLfunction toUTC
syntax keyword dSTLfunction toOtherTZ
syntax keyword dSTLfunction toUnixTime
syntax keyword dSTLfunction fromUnixTime
syntax keyword dSTLfunction toTimeVal
syntax keyword dSTLfunction toTimeSpec
syntax keyword dSTLfunction toTM
syntax keyword dSTLfunction add
syntax keyword dSTLfunction roll
syntax keyword dSTLfunction diffMonths
syntax keyword dSTLfunction isLeapYear
syntax keyword dSTLfunction dayOfWeek
syntax keyword dSTLfunction dayOfYear
syntax keyword dSTLfunction dayOfGregorianCal
syntax keyword dSTLfunction endOfMonth
syntax keyword dSTLfunction daysInMonth
syntax keyword dSTLfunction isAD
syntax keyword dSTLfunction julianDay
syntax keyword dSTLfunction modJulianDay
syntax keyword dSTLfunction toISOString
syntax keyword dSTLfunction toISOExtString
syntax keyword dSTLfunction toSimpleString
syntax keyword dSTLfunction fromISOString
syntax keyword dSTLfunction fromISOExtString
syntax keyword dSTLfunction fromSimpleString
syntax keyword dSTLfunction measureTime
syntax keyword dSTLfunction daysToDayOfWeek
syntax keyword dSTLfunction monthsToMonth
"std.demangle
syntax keyword dSTLfunction demangle
"std.encoding
syntax keyword dSTLenum INVALID_SEQUENCE
syntax keyword dSTLenum AsciiChar
syntax keyword dSTLenum Latin1Char
syntax keyword dSTLenum Latin1String
syntax keyword dSTLenum Latin2Char
syntax keyword dSTLenum Latin2String
syntax keyword dSTLenum Windows1250Char
syntax keyword dSTLenum Windows1250String
syntax keyword dSTLenum Windows1252Char
syntax keyword dSTLenum Windows1252String
syntax keyword dSTLenum BOM
syntax keyword dSTLenum utfBOM 
syntax keyword dSTLtype EncodingSchemeASCII
syntax keyword dSTLtype EncodingSchemeLatin1
syntax keyword dSTLtype EncodingSchemeLatin2
syntax keyword dSTLtype EncodingSchemeWindows1250
syntax keyword dSTLtype EncodingSchemeWindows1252
syntax keyword dSTLtype EncodingSchemeUtf8
syntax keyword dSTLtype EncodingSchemeUtf16Native
syntax keyword dSTLtype EncodingSchemeUtf32Native 
syntax keyword dSTLconstant bomTable 
syntax keyword dSTLfunction getBOM
syntax keyword dSTLfunction isValidCodePoint
syntax keyword dSTLfunction encodingName
syntax keyword dSTLfunction canEncode
syntax keyword dSTLfunction isValidCodeUnit
syntax keyword dSTLfunction isValid
syntax keyword dSTLfunction validLength
syntax keyword dSTLfunction sanitize
syntax keyword dSTLfunction firstSequence
syntax keyword dSTLfunction lastSequence
syntax keyword dSTLfunction index
syntax keyword dSTLfunction decodeReverse
syntax keyword dSTLfunction safeDecode
syntax keyword dSTLfunction encodedLength
syntax keyword dSTLfunction codePoints
syntax keyword dSTLfunction transcode
"std.file
syntax keyword dSTLfunction exists
syntax keyword dSTLfunction isDir
syntax keyword dSTLfunction isFile
syntax keyword dSTLfunction isSymlink
syntax keyword dSTLfunction rename
syntax keyword dSTLfunction thisExePath
syntax keyword dSTLfunction chdir
syntax keyword dSTLfunction dirEntries
syntax keyword dSTLfunction getcwd
syntax keyword dSTLfunction mkdir
syntax keyword dSTLfunction mkdirRecurse
syntax keyword dSTLfunction rmdir
syntax keyword dSTLfunction rmdirRecurse
syntax keyword dSTLfunction tempDir
syntax keyword dSTLfunction copy
syntax keyword dSTLfunction read
syntax keyword dSTLfunction readText
syntax keyword dSTLfunction remove
syntax keyword dSTLfunction slurp
syntax keyword dSTLfunction symlink
syntax keyword dSTLfunction readLink
syntax keyword dSTLfunction attrIsDir
syntax keyword dSTLfunction attrIsFile
syntax keyword dSTLfunction attrIsSymlink
syntax keyword dSTLfunction getAttributes
syntax keyword dSTLfunction getLinkAttributes
syntax keyword dSTLfunction getSize
syntax keyword dSTLfunction setAttributes
syntax keyword dSTLfunction getTimes
syntax keyword dSTLfunction getTimesWin
syntax keyword dSTLfunction setTimes
syntax keyword dSTLfunction timeLastModified
syntax keyword dSTLfunction formattedWrite
syntax keyword dSTLfunction formattedRead
syntax keyword dSTLfunction format
syntax keyword dSTLfunction sformat
syntax keyword dSTLfunction formatValue
syntax keyword dSTLfunction singleSpec
syntax keyword dSTLfunction unformatValue
"std.json
syntax keyword dSTLtype JSONValue
syntax keyword dSTLenum JSONFloatLiteral
syntax keyword dSTLenum JSONOptions
syntax keyword dSTLenum JSON_TYPE
syntax keyword dSTLenum CustomFloatFlags
"std.math
syntax keyword dSTLtype IeeeFlags
syntax keyword dSTLtype FloatingPointControl
syntax keyword dSTLconstant E
syntax keyword dSTLconstant PI
syntax keyword dSTLconstant PI_2
syntax keyword dSTLconstant PI_4
syntax keyword dSTLconstant M_1_PI
syntax keyword dSTLconstant M_2_PI
syntax keyword dSTLconstant M_2_SQRTPI
syntax keyword dSTLconstant LN10
syntax keyword dSTLconstant LN2
syntax keyword dSTLconstant LOG2
syntax keyword dSTLconstant LOG2E
syntax keyword dSTLconstant LOG2T
syntax keyword dSTLconstant LOG10E
syntax keyword dSTLconstant SQRT2
syntax keyword dSTLconstant SQRT1_2
syntax keyword dSTLfunction abs
syntax keyword dSTLfunction fabs
syntax keyword dSTLfunction sqrt
syntax keyword dSTLfunction cbrt
syntax keyword dSTLfunction hypot
syntax keyword dSTLfunction poly
syntax keyword dSTLfunction nextPow2
syntax keyword dSTLfunction truncPow2
syntax keyword dSTLfunction sin
syntax keyword dSTLfunction cos
syntax keyword dSTLfunction tan
syntax keyword dSTLfunction asin
syntax keyword dSTLfunction acos
syntax keyword dSTLfunction atan
syntax keyword dSTLfunction atan2
syntax keyword dSTLfunction sinh
syntax keyword dSTLfunction cosh
syntax keyword dSTLfunction tanh
syntax keyword dSTLfunction asinh
syntax keyword dSTLfunction acosh
syntax keyword dSTLfunction atanh
syntax keyword dSTLfunction expi
syntax keyword dSTLfunction ceil
syntax keyword dSTLfunction floor
syntax keyword dSTLfunction round
syntax keyword dSTLfunction lround
syntax keyword dSTLfunction trunc
syntax keyword dSTLfunction rint
syntax keyword dSTLfunction lrint
syntax keyword dSTLfunction nearbyint
syntax keyword dSTLfunction rndtol
syntax keyword dSTLfunction quantize
syntax keyword dSTLfunction pow
syntax keyword dSTLfunction exp
syntax keyword dSTLfunction exp2
syntax keyword dSTLfunction expm1
syntax keyword dSTLfunction ldexp
syntax keyword dSTLfunction frexp
syntax keyword dSTLfunction log
syntax keyword dSTLfunction log2
syntax keyword dSTLfunction log10
syntax keyword dSTLfunction logb
syntax keyword dSTLfunction ilogb
syntax keyword dSTLfunction log1p
syntax keyword dSTLfunction scalbn
syntax keyword dSTLfunction fmod
syntax keyword dSTLfunction modf
syntax keyword dSTLfunction remainder
syntax keyword dSTLfunction approxEqual
syntax keyword dSTLfunction feqrel
syntax keyword dSTLfunction fdim
syntax keyword dSTLfunction fmax
syntax keyword dSTLfunction fmin
syntax keyword dSTLfunction fma
syntax keyword dSTLfunction nextDown
syntax keyword dSTLfunction nextUp
syntax keyword dSTLfunction nextafter
syntax keyword dSTLfunction NaN
syntax keyword dSTLfunction getNaNPayload
syntax keyword dSTLfunction cmp
syntax keyword dSTLfunction isFinite
syntax keyword dSTLfunction isIdentical
syntax keyword dSTLfunction isInfinity
syntax keyword dSTLfunction isNaN
syntax keyword dSTLfunction isNormal
syntax keyword dSTLfunction isSubnormal
syntax keyword dSTLfunction signbit
syntax keyword dSTLfunction sgn
syntax keyword dSTLfunction copysign
syntax keyword dSTLfunction isPowerOf2
syntax keyword dSTLfunction conj
"std.meta
syntax keyword dSTLfunction Alias
syntax keyword dSTLfunction AliasSeq
syntax keyword dSTLfunction aliasSeqOf
syntax keyword dSTLfunction Erase
syntax keyword dSTLfunction EraseAll
syntax keyword dSTLfunction Filter
syntax keyword dSTLfunction NoDuplicates
syntax keyword dSTLfunction DerivedToFront
syntax keyword dSTLfunction MostDerived
syntax keyword dSTLfunction Repeat
syntax keyword dSTLfunction Replace
syntax keyword dSTLfunction ReplaceAll
syntax keyword dSTLfunction Reverse
syntax keyword dSTLfunction staticMap
syntax keyword dSTLfunction staticSort
syntax keyword dSTLfunction allSatisfy
syntax keyword dSTLfunction anySatisfy
syntax keyword dSTLfunction staticIndexOf
syntax keyword dSTLfunction templateAnd
syntax keyword dSTLfunction templateNot
syntax keyword dSTLfunction templateOr
syntax keyword dSTLfunction staticIsSorted
syntax keyword dSTLfunction ApplyLeft
syntax keyword dSTLfunction ApplyRight
"std.parallelism
syntax keyword dSTLtype TaskPool
syntax keyword dSTLconstant totalCPUs
syntax keyword dSTLfunction Task
syntax keyword dSTLfunction task
syntax keyword dSTLfunction taskPool
syntax keyword dSTLfunction defaultPoolThreads
syntax keyword dSTLfunction parallel
"std.path
syntax keyword dSTLenum dirSeparator
syntax keyword dSTLenum pathSeparator
syntax keyword dSTLenum CaseSensitive
syntax keyword dSTLfunction absolutePath
syntax keyword dSTLfunction asAbsolutePath
syntax keyword dSTLfunction asNormalizedPath
syntax keyword dSTLfunction asRelativePath
syntax keyword dSTLfunction buildNormalizedPath
syntax keyword dSTLfunction buildPath
syntax keyword dSTLfunction chainPath
syntax keyword dSTLfunction expandTilde
syntax keyword dSTLfunction baseName
syntax keyword dSTLfunction dirName
syntax keyword dSTLfunction dirSeparator
syntax keyword dSTLfunction driveName
syntax keyword dSTLfunction pathSeparator
syntax keyword dSTLfunction pathSplitter
syntax keyword dSTLfunction relativePath
syntax keyword dSTLfunction rootName
syntax keyword dSTLfunction stripDrive
syntax keyword dSTLfunction isAbsolute
syntax keyword dSTLfunction isDirSeparator
syntax keyword dSTLfunction isRooted
syntax keyword dSTLfunction isValidFilename
syntax keyword dSTLfunction isValidPath
syntax keyword dSTLfunction defaultExtension
syntax keyword dSTLfunction extension
syntax keyword dSTLfunction setExtension
syntax keyword dSTLfunction stripExtension
syntax keyword dSTLfunction withDefaultExtension
syntax keyword dSTLfunction withExtension
syntax keyword dSTLfunction filenameCharCmp
syntax keyword dSTLfunction filenameCmp
syntax keyword dSTLfunction globMatch
"std.stdint
syntax keyword dSTLtype int8_t
syntax keyword dSTLtype uint8_t
syntax keyword dSTLtype int16_t
syntax keyword dSTLtype uint16_t
syntax keyword dSTLtype int32_t
syntax keyword dSTLtype uint32_t
syntax keyword dSTLtype int64_t
syntax keyword dSTLtype uint64_t
syntax keyword dSTLtype intptr_t
syntax keyword dSTLtype uintptr_t
syntax keyword dSTLtype intmax_t
syntax keyword dSTLtype uintmax_t
syntax keyword dSTLtype BitRange
"std.stdio
syntax keyword dSTLtype File
syntax keyword dSTLtype lines
syntax keyword dSTLconstant stdin
syntax keyword dSTLconstant stdout
syntax keyword dSTLconstant stderr
syntax keyword dSTLconstant openNetwork
syntax keyword dSTLenum	LockType
syntax keyword dSTLfunction open
syntax keyword dSTLfunction reopen
syntax keyword dSTLfunction popen
syntax keyword dSTLfunction fdopen
syntax keyword dSTLfunction windowsHandleOpen
syntax keyword dSTLfunction isOpen
syntax keyword dSTLfunction eof
syntax keyword dSTLfunction name
syntax keyword dSTLfunction error
syntax keyword dSTLfunction detach
syntax keyword dSTLfunction close
syntax keyword dSTLfunction clearerr
syntax keyword dSTLfunction flush
syntax keyword dSTLfunction sync
syntax keyword dSTLfunction rawRead
syntax keyword dSTLfunction rawWrite
syntax keyword dSTLfunction seek
syntax keyword dSTLfunction tell
syntax keyword dSTLfunction rewind
syntax keyword dSTLfunction setvbuf
syntax keyword dSTLfunction lock
syntax keyword dSTLfunction tryLock
syntax keyword dSTLfunction unlock
syntax keyword dSTLfunction write
syntax keyword dSTLfunction writeln
syntax keyword dSTLfunction writef
syntax keyword dSTLfunction writefln
syntax keyword dSTLfunction readln
syntax keyword dSTLfunction readf
syntax keyword dSTLfunction tmpfile
syntax keyword dSTLfunction wrapFile
syntax keyword dSTLfunction getFP
syntax keyword dSTLfunction fileno
syntax keyword dSTLfunction windowsHandle
syntax keyword dSTLfunction byLine
syntax keyword dSTLfunction byLineCopy
syntax keyword dSTLfunction byRecord
syntax keyword dSTLfunction byChunk
syntax keyword dSTLfunction lockingTextWriter
syntax keyword dSTLfunction lockingBinaryWriter
syntax keyword dSTLfunction toFile
"std.system
syntax keyword dSTLconstant os
syntax keyword dSTLconstant endian
syntax keyword dSTLenum OS
syntax keyword dSTLenum Endian
"generic members
syntax keyword dSTLfunction popFront
syntax keyword dSTLfunction length
syntax keyword dSTLfunction reserve
syntax keyword dSTLfunction capacity
syntax keyword dSTLfunction size
syntax keyword dSTLfunction dup
syntax keyword dSTLfunction toHash
syntax keyword dSTLfunction toString
syntax keyword dSTLfunction append
syntax keyword dSTLfunction empty
syntax keyword dSTLfunction save
"core.atomic
syntax keyword dSTLenum MemoryOrder
syntax keyword dSTLfunction atomicOp
syntax keyword dSTLfunction cas
syntax keyword dSTLfunction atomicLoad
syntax keyword dSTLfunction atomicStore
syntax keyword dSTLfunction atomicFence
"core.bitop
syntax keyword dSTLfunction bsf
syntax keyword dSTLfunction bsr
syntax keyword dSTLfunction bt
syntax keyword dSTLfunction btc
syntax keyword dSTLfunction btr
syntax keyword dSTLfunction bts
syntax keyword dSTLfunction bitsPerWord
syntax keyword dSTLfunction bswap
"core.math
syntax keyword dSTLfunction yl2x
"core.cpuid
syntax keyword dSTLfunction CacheInfo

"Exceptions
syntax keyword dSTLexception MessageMismatch
syntax keyword dSTLexception OwnerTerminated
syntax keyword dSTLexception LinkTerminated
syntax keyword dSTLexception PriorityMessageException
syntax keyword dSTLexception MailboxFull
syntax keyword dSTLexception TidMissingException
syntax keyword dSTLexception ConvException
syntax keyword dSTLexception ConvOverflowException
syntax keyword dSTLexception CSVException
syntax keyword dSTLexception HeaderMismatchException
syntax keyword dSTLexception IncompleteCellException
syntax keyword dSTLexception DateTimeException
syntax keyword dSTLexception assertNotThrown
syntax keyword dSTLexception assertThrown
syntax keyword dSTLexception assumeUnique
syntax keyword dSTLexception assumeWontThrow
syntax keyword dSTLexception mayPointTo
syntax keyword dSTLexception doesPointTo
syntax keyword dSTLexception enforce
syntax keyword dSTLexception enforceEx
syntax keyword dSTLexception errnoEnforce
syntax keyword dSTLexception collectException
syntax keyword dSTLexception collectExceptionMsg
syntax keyword dSTLexception ifThrown
syntax keyword dSTLexception basicExceptionCtors
syntax keyword dSTLexception emptyExceptionMsg
syntax keyword dSTLexception ErrnoException
syntax keyword dSTLexception RangePrimitive
syntax keyword dSTLexception FileException
syntax keyword dSTLexception FormatException
syntax keyword dSTLexception GetOptException
syntax keyword dSTLexception JSONException
syntax keyword dSTLexception StdioException
syntax keyword dSTLexception Base64Exception

"std.functional
syntax keyword dSTLfunctional adjoin
syntax keyword dSTLfunctional compose
syntax keyword dSTLfunctional forward
syntax keyword dSTLfunctional lessThan
syntax keyword dSTLfunctional greaterThan
syntax keyword dSTLfunctional equalTo
syntax keyword dSTLfunctional memoize
syntax keyword dSTLfunctional not
syntax keyword dSTLfunctional partial
syntax keyword dSTLfunctional reverseArgs
syntax keyword dSTLfunctional toDelegate
syntax keyword dSTLfunctional unaryFun
syntax keyword dSTLfunctional binaryFun
syntax keyword dSTLfunctional binaryReverseArgs
syntax keyword dSTLfunctional pipe

" Default highlighting
if version >= 508 || !exists("did_d_syntax_inits")
	if version < 508
		let did_d_syntax_inits = 1
		command -nargs=+ HiLink hi link <args>
	else
		command -nargs=+ HiLink hi def link <args>
	endif
	HiLink dSTLbool         Boolean
	HiLink dStorageClass    StorageClass
	HiLink dStatement       Statement
	HiLink dSTLfunction     Function
	HiLink dSTLfunctional   Typedef
	HiLink dSTLconstant     Constant
	HiLink dSTLnamespace    Constant
	HiLink dSTLtype         Typedef
	HiLink dSTLexception    Exception
	HiLink dSTLenum         Typedef
	HiLink dSTLcast         Statement " be consistent with official syntax
	HiLink dRawString       String
	HiLink dRawDelimiter    Delimiter
	delcommand HiLink
endif
