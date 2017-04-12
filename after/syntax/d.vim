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
syn cluster dSTLgroup     contains=dPhobosFunction,dSTLfunctional,dPhobosConstant,dPhobosNamespace,dPhobosType,dPhobosException,,dPhobosEnum

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

syntax keyword dPhobosNamespace std
syntax keyword dPhobosNamespace etc
syntax keyword dPhobosNamespace core

"std.algorithm
syntax keyword dPhobosFunction all
syntax keyword dPhobosFunction any
syntax keyword dPhobosFunction balancedParens
syntax keyword dPhobosFunction boyerMooreFinder
syntax keyword dPhobosFunction canFind
syntax keyword dPhobosFunction commonPrefix
syntax keyword dPhobosFunction count
syntax keyword dPhobosFunction countUntil
syntax keyword dPhobosFunction endsWith
syntax keyword dPhobosFunction find
syntax keyword dPhobosFunction findAdjacent
syntax keyword dPhobosFunction findAmong
syntax keyword dPhobosFunction findSkip
syntax keyword dPhobosFunction findSplit
syntax keyword dPhobosFunction findSplitAfter
syntax keyword dPhobosFunction findSplitBefore
syntax keyword dPhobosFunction minCount
syntax keyword dPhobosFunction maxCount
syntax keyword dPhobosFunction minElement
syntax keyword dPhobosFunction maxElement
syntax keyword dPhobosFunction minIndex
syntax keyword dPhobosFunction maxIndex
syntax keyword dPhobosFunction minPos
syntax keyword dPhobosFunction maxPos
syntax keyword dPhobosFunction skipOver
syntax keyword dPhobosFunction startsWith
syntax keyword dPhobosFunction until
syntax keyword dPhobosFunction among
syntax keyword dPhobosFunction castSwitch
syntax keyword dPhobosFunction clamp
syntax keyword dPhobosFunction cmp
syntax keyword dPhobosFunction either
syntax keyword dPhobosFunction equal
syntax keyword dPhobosFunction isPermutation
syntax keyword dPhobosFunction isSameLength
syntax keyword dPhobosFunction levenshteinDistance
syntax keyword dPhobosFunction levenshteinDistanceAndPath
syntax keyword dPhobosFunction max
syntax keyword dPhobosFunction min
syntax keyword dPhobosFunction mismatch
syntax keyword dPhobosFunction predSwitch
syntax keyword dPhobosFunction cache
syntax keyword dPhobosFunction cacheBidirectional
syntax keyword dPhobosFunction chunkBy
syntax keyword dPhobosFunction cumulativeFold
syntax keyword dPhobosFunction each
syntax keyword dPhobosFunction filter
syntax keyword dPhobosFunction filterBidirectional
syntax keyword dPhobosFunction fold
syntax keyword dPhobosFunction group
syntax keyword dPhobosFunction joiner
syntax keyword dPhobosFunction map
syntax keyword dPhobosFunction permutations
syntax keyword dPhobosFunction reduce
syntax keyword dPhobosFunction splitter
syntax keyword dPhobosFunction sum
syntax keyword dPhobosFunction uniq
syntax keyword dPhobosFunction completeSort
syntax keyword dPhobosFunction isPartitioned
syntax keyword dPhobosFunction isSorted
syntax keyword dPhobosFunction isStrictlyMonotonic
syntax keyword dPhobosFunction ordered
syntax keyword dPhobosFunction strictlyOrdered
syntax keyword dPhobosFunction makeIndex
syntax keyword dPhobosFunction merge
syntax keyword dPhobosFunction multiSort
syntax keyword dPhobosFunction nextEvenPermutation
syntax keyword dPhobosFunction nextPermutation
syntax keyword dPhobosFunction partialSort
syntax keyword dPhobosFunction partition
syntax keyword dPhobosFunction partition3
syntax keyword dPhobosFunction schwartzSort
syntax keyword dPhobosFunction sort
syntax keyword dPhobosFunction topN
syntax keyword dPhobosFunction topNCopy
syntax keyword dPhobosFunction topNIndex
syntax keyword dPhobosFunction cartesianProduct
syntax keyword dPhobosFunction largestPartialIntersection
syntax keyword dPhobosFunction largestPartialIntersectionWeighted
syntax keyword dPhobosFunction nWayUnion
syntax keyword dPhobosFunction setDifference
syntax keyword dPhobosFunction setIntersection
syntax keyword dPhobosFunction setSymmetricDifference
syntax keyword dPhobosFunction bringToFront
syntax keyword dPhobosFunction copy
syntax keyword dPhobosFunction fill
syntax keyword dPhobosFunction initializeAll
syntax keyword dPhobosFunction move
syntax keyword dPhobosFunction moveAll
syntax keyword dPhobosFunction moveSome
syntax keyword dPhobosFunction moveEmplace
syntax keyword dPhobosFunction moveEmplaceAll
syntax keyword dPhobosFunction moveEmplaceSome
syntax keyword dPhobosFunction remove
syntax keyword dPhobosFunction reverse
syntax keyword dPhobosFunction strip
syntax keyword dPhobosFunction stripLeft
syntax keyword dPhobosFunction stripRight
syntax keyword dPhobosFunction swap
syntax keyword dPhobosFunction swapRanges
syntax keyword dPhobosFunction uninitializedFill
"std.array
syntax keyword dPhobosFunction array
syntax keyword dPhobosFunction appender
syntax keyword dPhobosFunction assocArray
syntax keyword dPhobosFunction byPair
syntax keyword dPhobosFunction insertInPlace
syntax keyword dPhobosFunction join
syntax keyword dPhobosFunction minimallyInitializedArray
syntax keyword dPhobosFunction replace
syntax keyword dPhobosFunction replaceFirst
syntax keyword dPhobosFunction replaceInPlace
syntax keyword dPhobosFunction replaceInto
syntax keyword dPhobosFunction replaceLast
syntax keyword dPhobosFunction replaceSlice
syntax keyword dPhobosFunction replicate
syntax keyword dPhobosFunction clear
syntax keyword dPhobosFunction shrinkTo
syntax keyword dPhobosFunction sameHead
syntax keyword dPhobosFunction sameTail
syntax keyword dPhobosFunction split
syntax keyword dPhobosFunction uninitializedArray
"std.ascii
syntax keyword dPhobosConstant digits
syntax keyword dPhobosConstant fullHexDigits
syntax keyword dPhobosConstant hexDigits
syntax keyword dPhobosConstant letters
syntax keyword dPhobosConstant lowercase
syntax keyword dPhobosConstant lowerHexDigits
syntax keyword dPhobosConstant newline
syntax keyword dPhobosConstant octalDigits
syntax keyword dPhobosConstant uppercase
syntax keyword dPhobosConstant whitespace
syntax keyword dPhobosFunction isAlpha
syntax keyword dPhobosFunction isAlphaNum
syntax keyword dPhobosFunction isASCII
syntax keyword dPhobosFunction isControl
syntax keyword dPhobosFunction isDigit
syntax keyword dPhobosFunction isGraphical
syntax keyword dPhobosFunction isHexDigit
syntax keyword dPhobosFunction isOctalDigit
syntax keyword dPhobosFunction isPrintable
syntax keyword dPhobosFunction isPunctuation
syntax keyword dPhobosFunction isUpper
syntax keyword dPhobosFunction isWhite
syntax keyword dPhobosFunction toLower
syntax keyword dPhobosFunction toUpper
"std.base64
syntax keyword dPhobosType Encoder
syntax keyword dPhobosType Decoder
syntax keyword dPhobosEnum NoPadding
syntax keyword dPhobosFunction encodeLength
syntax keyword dPhobosFunction encode
syntax keyword dPhobosFunction decodeLength
syntax keyword dPhobosFunction decode
syntax keyword dPhobosFunction encoder
syntax keyword dPhobosFunction decoder
"std.bigint
syntax keyword dPhobosType BigInt
syntax keyword dPhobosFunction toLong
syntax keyword dPhobosFunction toInt
syntax keyword dPhobosFunction uintLength
syntax keyword dPhobosFunction ulongLength
syntax keyword dPhobosFunction toDecimalString
syntax keyword dPhobosFunction toHex
syntax keyword dPhobosFunction absUnsign
"std.bitmanip
syntax keyword dPhobosType FloatRep
syntax keyword dPhobosType DoubleRep
syntax keyword dPhobosType BitArray
syntax keyword dPhobosFunction bitfields
syntax keyword dPhobosFunction bitsSet
syntax keyword dPhobosFunction bigEndianToNative
syntax keyword dPhobosFunction littleEndianToNative
syntax keyword dPhobosFunction nativeToBigEndian
syntax keyword dPhobosFunction nativeToLittleEndian
syntax keyword dPhobosFunction swapEndian
syntax keyword dPhobosFunction peek
syntax keyword dPhobosFunction read
syntax keyword dPhobosFunction taggedClassRef
syntax keyword dPhobosFunction taggedPointer
"std.containers
syntax keyword dPhobosType Array
syntax keyword dPhobosType BinaryHeap
syntax keyword dPhobosType DList
syntax keyword dPhobosType RedBlackTree
syntax keyword dPhobosType SList
syntax keyword dPhobosType RefAppender
"std.complex
syntax keyword dPhobosType Complex
syntax keyword dPhobosFunction arg
syntax keyword dPhobosFunction complex
syntax keyword dPhobosFunction conj
syntax keyword dPhobosFunction fromPolar
syntax keyword dPhobosFunction sqAbs
"std.concurrency
syntax keyword dPhobosType Tid
syntax keyword dPhobosType ThreadInfo
syntax keyword dPhobosType ThreadScheduler
syntax keyword dPhobosType FiberScheduler
syntax keyword dPhobosType Generator
syntax keyword dPhobosEnum OnCrowding
syntax keyword dPhobosFunction start
syntax keyword dPhobosFunction newCondition
syntax keyword dPhobosFunction thisInfo
syntax keyword dPhobosFunction cleanup
syntax keyword dPhobosFunction initOnce
syntax keyword dPhobosFunction locate
syntax keyword dPhobosFunction ownerTid
syntax keyword dPhobosFunction prioritySend
syntax keyword dPhobosFunction receive
syntax keyword dPhobosFunction receiveOnly
syntax keyword dPhobosFunction receiveTimeout
syntax keyword dPhobosFunction register
syntax keyword dPhobosFunction Scheduler
syntax keyword dPhobosFunction scheduler
syntax keyword dPhobosFunction send
syntax keyword dPhobosFunction setMaxMailboxSize
syntax keyword dPhobosFunction spawn
syntax keyword dPhobosFunction spawnLinked
syntax keyword dPhobosFunction thisTid
syntax keyword dPhobosFunction unregister
syntax keyword dPhobosFunction yield
"std.compiler
syntax keyword dPhobosConstant name
syntax keyword dPhobosConstant vendor
syntax keyword dPhobosConstant version_major
syntax keyword dPhobosConstant version_minor
syntax keyword dPhobosConstant D_major
syntax keyword dPhobosEnum Vendor
"std.conv
syntax keyword dPhobosFunction castFrom
syntax keyword dPhobosFunction emplace
syntax keyword dPhobosFunction parse
syntax keyword dPhobosFunction to
syntax keyword dPhobosFunction toChars
syntax keyword dPhobosFunction text
syntax keyword dPhobosFunction wtext
syntax keyword dPhobosFunction dtext
syntax keyword dPhobosFunction hexString
syntax keyword dPhobosFunction octal
syntax keyword dPhobosFunction roundTo
syntax keyword dPhobosFunction signed
syntax keyword dPhobosFunction unsigned
"std.csv
syntax keyword dPhobosEnum Malformed
syntax keyword dPhobosFunction csvNextToken
syntax keyword dPhobosFunction csvReader
"std.datetime
syntax keyword dPhobosType SysTime
syntax keyword dPhobosType Date
syntax keyword dPhobosType TimeOfDay
syntax keyword dPhobosType DateTime
syntax keyword dPhobosType FormatSpec
syntax keyword dPhobosEnum LetterCase
syntax keyword dPhobosEnum Month
syntax keyword dPhobosEnum DayOfWeek
syntax keyword dPhobosEnum Direction
syntax keyword dPhobosConstant timeStrings
syntax keyword dPhobosFunction currTime
syntax keyword dPhobosFunction currStdTime
syntax keyword dPhobosFunction year
syntax keyword dPhobosFunction yearBC
syntax keyword dPhobosFunction month
syntax keyword dPhobosFunction day
syntax keyword dPhobosFunction hour
syntax keyword dPhobosFunction minute
syntax keyword dPhobosFunction second
syntax keyword dPhobosFunction fracSecs
syntax keyword dPhobosFunction stdTime
syntax keyword dPhobosFunction timezone
syntax keyword dPhobosFunction dstInEffect
syntax keyword dPhobosFunction utcOffset
syntax keyword dPhobosFunction toLocalTime
syntax keyword dPhobosFunction toUTC
syntax keyword dPhobosFunction toOtherTZ
syntax keyword dPhobosFunction toUnixTime
syntax keyword dPhobosFunction fromUnixTime
syntax keyword dPhobosFunction toTimeVal
syntax keyword dPhobosFunction toTimeSpec
syntax keyword dPhobosFunction toTM
syntax keyword dPhobosFunction add
syntax keyword dPhobosFunction roll
syntax keyword dPhobosFunction diffMonths
syntax keyword dPhobosFunction isLeapYear
syntax keyword dPhobosFunction dayOfWeek
syntax keyword dPhobosFunction dayOfYear
syntax keyword dPhobosFunction dayOfGregorianCal
syntax keyword dPhobosFunction endOfMonth
syntax keyword dPhobosFunction daysInMonth
syntax keyword dPhobosFunction isAD
syntax keyword dPhobosFunction julianDay
syntax keyword dPhobosFunction modJulianDay
syntax keyword dPhobosFunction toISOString
syntax keyword dPhobosFunction toISOExtString
syntax keyword dPhobosFunction toSimpleString
syntax keyword dPhobosFunction fromISOString
syntax keyword dPhobosFunction fromISOExtString
syntax keyword dPhobosFunction fromSimpleString
syntax keyword dPhobosFunction measureTime
syntax keyword dPhobosFunction daysToDayOfWeek
syntax keyword dPhobosFunction monthsToMonth
"std.demangle
syntax keyword dPhobosFunction demangle
"std.encoding
syntax keyword dPhobosEnum INVALID_SEQUENCE
syntax keyword dPhobosEnum AsciiChar
syntax keyword dPhobosEnum Latin1Char
syntax keyword dPhobosEnum Latin1String
syntax keyword dPhobosEnum Latin2Char
syntax keyword dPhobosEnum Latin2String
syntax keyword dPhobosEnum Windows1250Char
syntax keyword dPhobosEnum Windows1250String
syntax keyword dPhobosEnum Windows1252Char
syntax keyword dPhobosEnum Windows1252String
syntax keyword dPhobosEnum BOM
syntax keyword dPhobosEnum utfBOM
syntax keyword dPhobosType EncodingSchemeASCII
syntax keyword dPhobosType EncodingSchemeLatin1
syntax keyword dPhobosType EncodingSchemeLatin2
syntax keyword dPhobosType EncodingSchemeWindows1250
syntax keyword dPhobosType EncodingSchemeWindows1252
syntax keyword dPhobosType EncodingSchemeUtf8
syntax keyword dPhobosType EncodingSchemeUtf16Native
syntax keyword dPhobosType EncodingSchemeUtf32Native
syntax keyword dPhobosConstant bomTable
syntax keyword dPhobosFunction getBOM
syntax keyword dPhobosFunction isValidCodePoint
syntax keyword dPhobosFunction encodingName
syntax keyword dPhobosFunction canEncode
syntax keyword dPhobosFunction isValidCodeUnit
syntax keyword dPhobosFunction isValid
syntax keyword dPhobosFunction validLength
syntax keyword dPhobosFunction sanitize
syntax keyword dPhobosFunction firstSequence
syntax keyword dPhobosFunction lastSequence
syntax keyword dPhobosFunction index
syntax keyword dPhobosFunction decodeReverse
syntax keyword dPhobosFunction safeDecode
syntax keyword dPhobosFunction encodedLength
syntax keyword dPhobosFunction codePoints
syntax keyword dPhobosFunction transcode
"std.file
syntax keyword dPhobosFunction exists
syntax keyword dPhobosFunction isDir
syntax keyword dPhobosFunction isFile
syntax keyword dPhobosFunction isSymlink
syntax keyword dPhobosFunction rename
syntax keyword dPhobosFunction thisExePath
syntax keyword dPhobosFunction chdir
syntax keyword dPhobosFunction dirEntries
syntax keyword dPhobosFunction getcwd
syntax keyword dPhobosFunction mkdir
syntax keyword dPhobosFunction mkdirRecurse
syntax keyword dPhobosFunction rmdir
syntax keyword dPhobosFunction rmdirRecurse
syntax keyword dPhobosFunction tempDir
syntax keyword dPhobosFunction copy
syntax keyword dPhobosFunction read
syntax keyword dPhobosFunction readText
syntax keyword dPhobosFunction remove
syntax keyword dPhobosFunction slurp
syntax keyword dPhobosFunction symlink
syntax keyword dPhobosFunction readLink
syntax keyword dPhobosFunction attrIsDir
syntax keyword dPhobosFunction attrIsFile
syntax keyword dPhobosFunction attrIsSymlink
syntax keyword dPhobosFunction getAttributes
syntax keyword dPhobosFunction getLinkAttributes
syntax keyword dPhobosFunction getSize
syntax keyword dPhobosFunction setAttributes
syntax keyword dPhobosFunction getTimes
syntax keyword dPhobosFunction getTimesWin
syntax keyword dPhobosFunction setTimes
syntax keyword dPhobosFunction timeLastModified
syntax keyword dPhobosFunction formattedWrite
syntax keyword dPhobosFunction formattedRead
syntax keyword dPhobosFunction format
syntax keyword dPhobosFunction sformat
syntax keyword dPhobosFunction formatValue
syntax keyword dPhobosFunction singleSpec
syntax keyword dPhobosFunction unformatValue
"std.json
syntax keyword dPhobosType JSONValue
syntax keyword dPhobosEnum JSONFloatLiteral
syntax keyword dPhobosEnum JSONOptions
syntax keyword dPhobosEnum JSON_TYPE
syntax keyword dPhobosEnum CustomFloatFlags
"std.math
syntax keyword dPhobosType IeeeFlags
syntax keyword dPhobosType FloatingPointControl
syntax keyword dPhobosConstant E
syntax keyword dPhobosConstant PI
syntax keyword dPhobosConstant PI_2
syntax keyword dPhobosConstant PI_4
syntax keyword dPhobosConstant M_1_PI
syntax keyword dPhobosConstant M_2_PI
syntax keyword dPhobosConstant M_2_SQRTPI
syntax keyword dPhobosConstant LN10
syntax keyword dPhobosConstant LN2
syntax keyword dPhobosConstant LOG2
syntax keyword dPhobosConstant LOG2E
syntax keyword dPhobosConstant LOG2T
syntax keyword dPhobosConstant LOG10E
syntax keyword dPhobosConstant SQRT2
syntax keyword dPhobosConstant SQRT1_2
syntax keyword dPhobosFunction abs
syntax keyword dPhobosFunction fabs
syntax keyword dPhobosFunction sqrt
syntax keyword dPhobosFunction cbrt
syntax keyword dPhobosFunction hypot
syntax keyword dPhobosFunction poly
syntax keyword dPhobosFunction nextPow2
syntax keyword dPhobosFunction truncPow2
syntax keyword dPhobosFunction sin
syntax keyword dPhobosFunction cos
syntax keyword dPhobosFunction tan
syntax keyword dPhobosFunction asin
syntax keyword dPhobosFunction acos
syntax keyword dPhobosFunction atan
syntax keyword dPhobosFunction atan2
syntax keyword dPhobosFunction sinh
syntax keyword dPhobosFunction cosh
syntax keyword dPhobosFunction tanh
syntax keyword dPhobosFunction asinh
syntax keyword dPhobosFunction acosh
syntax keyword dPhobosFunction atanh
syntax keyword dPhobosFunction expi
syntax keyword dPhobosFunction ceil
syntax keyword dPhobosFunction floor
syntax keyword dPhobosFunction round
syntax keyword dPhobosFunction lround
syntax keyword dPhobosFunction trunc
syntax keyword dPhobosFunction rint
syntax keyword dPhobosFunction lrint
syntax keyword dPhobosFunction nearbyint
syntax keyword dPhobosFunction rndtol
syntax keyword dPhobosFunction quantize
syntax keyword dPhobosFunction pow
syntax keyword dPhobosFunction exp
syntax keyword dPhobosFunction exp2
syntax keyword dPhobosFunction expm1
syntax keyword dPhobosFunction ldexp
syntax keyword dPhobosFunction frexp
syntax keyword dPhobosFunction log
syntax keyword dPhobosFunction log2
syntax keyword dPhobosFunction log10
syntax keyword dPhobosFunction logb
syntax keyword dPhobosFunction ilogb
syntax keyword dPhobosFunction log1p
syntax keyword dPhobosFunction scalbn
syntax keyword dPhobosFunction fmod
syntax keyword dPhobosFunction modf
syntax keyword dPhobosFunction remainder
syntax keyword dPhobosFunction approxEqual
syntax keyword dPhobosFunction feqrel
syntax keyword dPhobosFunction fdim
syntax keyword dPhobosFunction fmax
syntax keyword dPhobosFunction fmin
syntax keyword dPhobosFunction fma
syntax keyword dPhobosFunction nextDown
syntax keyword dPhobosFunction nextUp
syntax keyword dPhobosFunction nextafter
syntax keyword dPhobosFunction NaN
syntax keyword dPhobosFunction getNaNPayload
syntax keyword dPhobosFunction cmp
syntax keyword dPhobosFunction isFinite
syntax keyword dPhobosFunction isIdentical
syntax keyword dPhobosFunction isInfinity
syntax keyword dPhobosFunction isNaN
syntax keyword dPhobosFunction isNormal
syntax keyword dPhobosFunction isSubnormal
syntax keyword dPhobosFunction signbit
syntax keyword dPhobosFunction sgn
syntax keyword dPhobosFunction copysign
syntax keyword dPhobosFunction isPowerOf2
syntax keyword dPhobosFunction conj
"std.meta
syntax keyword dPhobosFunction Alias
syntax keyword dPhobosFunction AliasSeq
syntax keyword dPhobosFunction aliasSeqOf
syntax keyword dPhobosFunction Erase
syntax keyword dPhobosFunction EraseAll
syntax keyword dPhobosFunction Filter
syntax keyword dPhobosFunction NoDuplicates
syntax keyword dPhobosFunction DerivedToFront
syntax keyword dPhobosFunction MostDerived
syntax keyword dPhobosFunction Repeat
syntax keyword dPhobosFunction Replace
syntax keyword dPhobosFunction ReplaceAll
syntax keyword dPhobosFunction Reverse
syntax keyword dPhobosFunction staticMap
syntax keyword dPhobosFunction staticSort
syntax keyword dPhobosFunction allSatisfy
syntax keyword dPhobosFunction anySatisfy
syntax keyword dPhobosFunction staticIndexOf
syntax keyword dPhobosFunction templateAnd
syntax keyword dPhobosFunction templateNot
syntax keyword dPhobosFunction templateOr
syntax keyword dPhobosFunction staticIsSorted
syntax keyword dPhobosFunction ApplyLeft
syntax keyword dPhobosFunction ApplyRight
"std.parallelism
syntax keyword dPhobosType TaskPool
syntax keyword dPhobosConstant totalCPUs
syntax keyword dPhobosFunction Task
syntax keyword dPhobosFunction task
syntax keyword dPhobosFunction taskPool
syntax keyword dPhobosFunction defaultPoolThreads
syntax keyword dPhobosFunction parallel
"std.path
syntax keyword dPhobosEnum dirSeparator
syntax keyword dPhobosEnum pathSeparator
syntax keyword dPhobosEnum CaseSensitive
syntax keyword dPhobosFunction absolutePath
syntax keyword dPhobosFunction asAbsolutePath
syntax keyword dPhobosFunction asNormalizedPath
syntax keyword dPhobosFunction asRelativePath
syntax keyword dPhobosFunction buildNormalizedPath
syntax keyword dPhobosFunction buildPath
syntax keyword dPhobosFunction chainPath
syntax keyword dPhobosFunction expandTilde
syntax keyword dPhobosFunction baseName
syntax keyword dPhobosFunction dirName
syntax keyword dPhobosFunction dirSeparator
syntax keyword dPhobosFunction driveName
syntax keyword dPhobosFunction pathSeparator
syntax keyword dPhobosFunction pathSplitter
syntax keyword dPhobosFunction relativePath
syntax keyword dPhobosFunction rootName
syntax keyword dPhobosFunction stripDrive
syntax keyword dPhobosFunction isAbsolute
syntax keyword dPhobosFunction isDirSeparator
syntax keyword dPhobosFunction isRooted
syntax keyword dPhobosFunction isValidFilename
syntax keyword dPhobosFunction isValidPath
syntax keyword dPhobosFunction defaultExtension
syntax keyword dPhobosFunction extension
syntax keyword dPhobosFunction setExtension
syntax keyword dPhobosFunction stripExtension
syntax keyword dPhobosFunction withDefaultExtension
syntax keyword dPhobosFunction withExtension
syntax keyword dPhobosFunction filenameCharCmp
syntax keyword dPhobosFunction filenameCmp
syntax keyword dPhobosFunction globMatch
"std.stdint
syntax keyword dPhobosType int8_t
syntax keyword dPhobosType uint8_t
syntax keyword dPhobosType int16_t
syntax keyword dPhobosType uint16_t
syntax keyword dPhobosType int32_t
syntax keyword dPhobosType uint32_t
syntax keyword dPhobosType int64_t
syntax keyword dPhobosType uint64_t
syntax keyword dPhobosType intptr_t
syntax keyword dPhobosType uintptr_t
syntax keyword dPhobosType intmax_t
syntax keyword dPhobosType uintmax_t
syntax keyword dPhobosType BitRange
"std.stdio
syntax keyword dPhobosType File
syntax keyword dPhobosType lines
syntax keyword dPhobosConstant stdin
syntax keyword dPhobosConstant stdout
syntax keyword dPhobosConstant stderr
syntax keyword dPhobosConstant openNetwork
syntax keyword dPhobosEnum	LockType
syntax keyword dPhobosFunction open
syntax keyword dPhobosFunction reopen
syntax keyword dPhobosFunction popen
syntax keyword dPhobosFunction fdopen
syntax keyword dPhobosFunction windowsHandleOpen
syntax keyword dPhobosFunction isOpen
syntax keyword dPhobosFunction eof
syntax keyword dPhobosFunction name
syntax keyword dPhobosFunction error
syntax keyword dPhobosFunction detach
syntax keyword dPhobosFunction close
syntax keyword dPhobosFunction clearerr
syntax keyword dPhobosFunction flush
syntax keyword dPhobosFunction sync
syntax keyword dPhobosFunction rawRead
syntax keyword dPhobosFunction rawWrite
syntax keyword dPhobosFunction seek
syntax keyword dPhobosFunction tell
syntax keyword dPhobosFunction rewind
syntax keyword dPhobosFunction setvbuf
syntax keyword dPhobosFunction lock
syntax keyword dPhobosFunction tryLock
syntax keyword dPhobosFunction unlock
syntax keyword dPhobosFunction write
syntax keyword dPhobosFunction writeln
syntax keyword dPhobosFunction writef
syntax keyword dPhobosFunction writefln
syntax keyword dPhobosFunction readln
syntax keyword dPhobosFunction readf
syntax keyword dPhobosFunction tmpfile
syntax keyword dPhobosFunction wrapFile
syntax keyword dPhobosFunction getFP
syntax keyword dPhobosFunction fileno
syntax keyword dPhobosFunction windowsHandle
syntax keyword dPhobosFunction byLine
syntax keyword dPhobosFunction byLineCopy
syntax keyword dPhobosFunction byRecord
syntax keyword dPhobosFunction byChunk
syntax keyword dPhobosFunction lockingTextWriter
syntax keyword dPhobosFunction lockingBinaryWriter
syntax keyword dPhobosFunction toFile
"std.system
syntax keyword dPhobosConstant os
syntax keyword dPhobosConstant endian
syntax keyword dPhobosEnum OS
syntax keyword dPhobosEnum Endian
"generic members
syntax keyword dPhobosFunction popFront
syntax keyword dPhobosFunction length
syntax keyword dPhobosFunction reserve
syntax keyword dPhobosFunction capacity
syntax keyword dPhobosFunction size
syntax keyword dPhobosFunction dup
syntax keyword dPhobosFunction toHash
syntax keyword dPhobosFunction toString
syntax keyword dPhobosFunction append
syntax keyword dPhobosFunction empty
syntax keyword dPhobosFunction save
"core.atomic
syntax keyword dPhobosEnum MemoryOrder
syntax keyword dPhobosFunction atomicOp
syntax keyword dPhobosFunction cas
syntax keyword dPhobosFunction atomicLoad
syntax keyword dPhobosFunction atomicStore
syntax keyword dPhobosFunction atomicFence
"core.bitop
syntax keyword dPhobosFunction bsf
syntax keyword dPhobosFunction bsr
syntax keyword dPhobosFunction bt
syntax keyword dPhobosFunction btc
syntax keyword dPhobosFunction btr
syntax keyword dPhobosFunction bts
syntax keyword dPhobosFunction bitsPerWord
syntax keyword dPhobosFunction bswap
"core.math
syntax keyword dPhobosFunction yl2x
"core.cpuid
syntax keyword dPhobosFunction CacheInfo

"Exceptions
syntax keyword dPhobosException MessageMismatch
syntax keyword dPhobosException OwnerTerminated
syntax keyword dPhobosException LinkTerminated
syntax keyword dPhobosException PriorityMessageException
syntax keyword dPhobosException MailboxFull
syntax keyword dPhobosException TidMissingException
syntax keyword dPhobosException ConvException
syntax keyword dPhobosException ConvOverflowException
syntax keyword dPhobosException CSVException
syntax keyword dPhobosException HeaderMismatchException
syntax keyword dPhobosException IncompleteCellException
syntax keyword dPhobosException DateTimeException
syntax keyword dPhobosException assertNotThrown
syntax keyword dPhobosException assertThrown
syntax keyword dPhobosException assumeUnique
syntax keyword dPhobosException assumeWontThrow
syntax keyword dPhobosException mayPointTo
syntax keyword dPhobosException doesPointTo
syntax keyword dPhobosException enforce
syntax keyword dPhobosException enforceEx
syntax keyword dPhobosException errnoEnforce
syntax keyword dPhobosException collectException
syntax keyword dPhobosException collectExceptionMsg
syntax keyword dPhobosException ifThrown
syntax keyword dPhobosException basicExceptionCtors
syntax keyword dPhobosException emptyExceptionMsg
syntax keyword dPhobosException ErrnoException
syntax keyword dPhobosException RangePrimitive
syntax keyword dPhobosException FileException
syntax keyword dPhobosException FormatException
syntax keyword dPhobosException GetOptException
syntax keyword dPhobosException JSONException
syntax keyword dPhobosException StdioException
syntax keyword dPhobosException Base64Exception

"std.functional
syntax keyword dPhobosFunctional adjoin
syntax keyword dPhobosFunctional compose
syntax keyword dPhobosFunctional forward
syntax keyword dPhobosFunctional lessThan
syntax keyword dPhobosFunctional greaterThan
syntax keyword dPhobosFunctional equalTo
syntax keyword dPhobosFunctional memoize
syntax keyword dPhobosFunctional not
syntax keyword dPhobosFunctional partial
syntax keyword dPhobosFunctional reverseArgs
syntax keyword dPhobosFunctional toDelegate
syntax keyword dPhobosFunctional unaryFun
syntax keyword dPhobosFunctional binaryFun
syntax keyword dPhobosFunctional binaryReverseArgs
syntax keyword dPhobosFunctional pipe

" Default highlighting
if version >= 508 || !exists("did_d_syntax_inits")
	if version < 508
		let did_d_syntax_inits = 1
		command -nargs=+ HiLink hi link <args>
	else
		command -nargs=+ HiLink hi def link <args>
	endif
	HiLink dPhobosBool         Boolean
	HiLink dStorageClass    StorageClass
	HiLink dStatement       Statement
	HiLink dPhobosFunction     Function
	HiLink dPhobosFunctional   Typedef
	HiLink dPhobosConstant     Constant
	HiLink dPhobosNamespace    Constant
	HiLink dPhobosType         Typedef
	HiLink dPhobosException    Exception
	HiLink dPhobosEnum         Typedef
	HiLink dPhobosCast         Statement " be consistent with official syntax
	HiLink dRawString       String
	HiLink dRawDelimiter    Delimiter
	delcommand HiLink
endif
