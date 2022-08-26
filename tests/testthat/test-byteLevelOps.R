
test_that("midi var length encoding", {
# Midi Spec ...
# bit 0 means the least significant bit of a byte, and bit 7 is the most significant.
# Some numbers in MIDI Files are represented in a form called a variable-length quantity.
# These numbers are represented 7 bits per byte, most significant bits first.
# All bytes except the last have bit 7 set, and the last byte has bit 7 clear.
# If the number is between 0 and 127, it is thus represented exactly as one byte.

# 00000000 00
# 00000040 40
# 0000007F 7F
# 00000080 81 00
# 00002000 C0 00
# 00003FFF FF 7F
# 00004000 81 80 00
# 00100000 C0 80 00
# 001FFFFF FF FF 7F
# 00200000 81 80 80 00
# 08000000 C0 80 80 00
# 0FFFFFFF FF FF FF 7F

  expect_equal( bytesVarLenInt(0x00),   as.raw(0x00) )
  expect_equal( bytesVarLenInt(0x40),   as.raw(0x40) )
  expect_equal( bytesVarLenInt(0x7F),   as.raw(0x7F) )
  expect_equal( bytesVarLenInt(0x80),   as.raw(c(0x81,0x00)) )
  expect_equal( bytesVarLenInt(0x2000), as.raw(c(0xC0,0x00)) )
  expect_equal( bytesVarLenInt(0x4000), as.raw(c(0x81,0x80,0x00) ) )
  expect_equal( bytesVarLenInt(0x100000),   as.raw(c(0xC0,0x80,0x00) ) )
  expect_equal( bytesVarLenInt(0x1FFFFF),   as.raw(c(0xFF,0xFF,0x7f) ) )
  expect_equal( bytesVarLenInt(0x200000),   as.raw(c(0x81,0x80,0x80,0x00) ) )
  expect_equal( bytesVarLenInt(0x8000000),   as.raw(c(0xC0,0x80,0x80,0x00) ) )
  expect_equal( bytesVarLenInt(0xFFFFFFF),   as.raw(c(0xFF,0xFF,0xFF,0x7F) ) )
})


test_that("short integer encoding", {
  #Int to two byte encoded
  #Most significant bit first

  expect_equal( bytesShort(0),   as.raw(c(0x00,0x00)) )
  expect_equal( bytesShort(15),   as.raw(c(0x00,0x0f)) )
  expect_equal( bytesShort(4095),   as.raw(c(0x0F,0xFF)) )
  expect_equal( bytesShort(65535),   as.raw(c(0xFF,0xFF)) )
  expect_equal( bytesShort(65536),   as.raw(c(0x00,0x00)) )  # should throw error
})

test_that("long integer unsigned encoding", {
  #Int to four byte encoded
  #Most significant bit first

  expect_equal( bytesInt(0),   as.raw(c(0x00,0x00,0x00,0x00)) )
  expect_equal( bytesInt(16),   as.raw(c(0x00,0x00,0x00,0x10)) )
  expect_equal( bytesInt(4095),   as.raw(c(0x00,0x00,0x0F,0xFF)) )
  expect_equal( bytesInt(524287),   as.raw(c(0x00,0x07,0xFF,0xFF)) )
  expect_equal( bytesInt(2147483647),   as.raw(c(0x7F,0xFF,0xFF,0xFF)) )
  expect_equal( bytesInt(0x7FFFFFFF),   as.raw(c(0x7F,0xFF,0xFF,0xFF)) )
#  expect_warning( bytesInt(0xFFFFFFFF),class = "simpleWarning" )

})

test_that("bytes to characters", {
  #Int to four byte encoded
  #Most significant bit first

  expect_equal( bytesChar(""),   raw(0) )
  expect_equal( bytesChar("a"),   as.raw(c(0x61)) )
  expect_equal( bytesChar("Test"),   as.raw(c(0x54,0x65,0x73,0x74)) )

})


