package com.josephus

import org.scalatest.prop.PropertyChecks
import org.scalatest.{PropSpec, ShouldMatchers}


class JosephusSpec extends PropSpec with PropertyChecks with ShouldMatchers {

  val josephusParams =
    Table(("noPeople", "stepRate", "startingPosition", "expectedValue"),
      (15, 2, 1, 15),
      (16, 2, 1, 1),
      (63, 3, 1, 50),
      (100, 10, 1, 26),
      (10, 2, 7, 2),
      (10, 4, 7, 2),
      (10, 10, 1, 8)
    )

  property("For given parameters, the expected survivor is correct") {

    forAll(josephusParams) { (noPeople: Int, stepRate: Int, startingPosition: Int, expectedValue: Int) =>
      assertResult(List(expectedValue)) {
        new josephus(noPeople, stepRate, startingPosition).findSurvivor().values
      }
    }
  }
}
