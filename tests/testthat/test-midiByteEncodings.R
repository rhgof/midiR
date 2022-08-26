
test_that("Midi Channel", {
  # FF 20 01 cc MIDI Channel Prefix -------
  # cc = 0-15
  expect_equal(bytesChannel(), as.raw(c(0xFF,0x20,0x01,0x00,0x00)))
  expect_equal(bytesChannel(1), as.raw(c(0xFF,0x20,0x01,0x00,0x00)))
  expect_equal(bytesChannel(16), as.raw(c(0xFF,0x20,0x01,0x0f,0x00)))


  expect_warning(bytesChannel(17))
  expect_warning(bytesChannel(-1))
  })

test_that("Midi Note On Off Tests", {

  # pitch = 48
  # velocity = 100
  bytePitch = as.raw(48)
  byteVelocity = as.raw(100)

  noteOn = TRUE
  noteOff = FALSE
  byteNoteOn = as.raw(0x90)
  byteNoteOff = as.raw(0x80)
  channel=0
  byteChannel = as.raw(channel)

  deltaTime=0
  expect_equal(bytesMidiNote(48,100,deltaTime,noteOn), as.raw(c(bytesVarLenInt(deltaTime),byteNoteOn | byteChannel,bytePitch,byteVelocity)))

  deltaTime=0
  expect_equal(bytesMidiNote(48,100,deltaTime,noteOff), as.raw(c(bytesVarLenInt(deltaTime),byteNoteOff | byteChannel,bytePitch,byteVelocity)))

  deltaTime=5760
  expect_equal(bytesMidiNote(48,100,deltaTime,noteOn), as.raw(c(bytesVarLenInt(deltaTime),byteNoteOn | byteChannel,bytePitch,byteVelocity)))

  deltaTime=5760
  expect_equal(bytesMidiNote(48,100,deltaTime,noteOff), as.raw(c(bytesVarLenInt(deltaTime),byteNoteOff | byteChannel,bytePitch,byteVelocity)))

  deltaTime=5760
  channel=2
  byteChannel = as.raw(channel-1) # from midi # to bytes
  expect_equal(bytesMidiNote(48,100,deltaTime,noteOff,channel), as.raw(c(bytesVarLenInt(deltaTime),byteNoteOff | byteChannel,bytePitch,byteVelocity)))
  expect_equal(bytesMidiNote(48,100,deltaTime,noteOn,channel), as.raw(c(bytesVarLenInt(deltaTime),byteNoteOn | byteChannel,bytePitch,byteVelocity)))

})

