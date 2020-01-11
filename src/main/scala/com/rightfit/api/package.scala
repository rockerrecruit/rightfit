package com.rightfit

import zio.RIO

package object api {
  def loadConfig: RIO[Configuration, Config] = RIO.accessM(_.config.load)
}
