package org.goldenport.context

import org.goldenport.exception.GoldenportException

/*
 * @since   Oct. 12, 2021
 *  version Oct. 12, 2021
 * @version Nov. 15, 2021
 * @author  ASAMI, Tomoharu
 */
class ConclusionException(val conclusion: Conclusion)
    extends GoldenportException(conclusion.message) {
  def message = conclusion.message
}
