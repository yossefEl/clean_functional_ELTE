definition module Data.Queue

/**
 * This module provides a straightforward FIFO queue.
 * It is implemented using two list based on Chris Okasaki's example in Purely
 * Functional Data Structures.
 */

import _SystemStrictLists
from StdOverloaded import class length

:: Queue a = Queue ![!a!] ![!a!]

/**
 * Create an empty queue
 */
newQueue :: Queue a

/**
 * Test if the queue is empty
 * @type (Queue a) -> Bool
 */
empty q :== q=:(Queue [|] [|])

instance length Queue

/**
 * Add an element to the queue
 */
enqueue :: a !(Queue a) -> Queue a

/**
 * Take an element from the queue (if the queue is not empty)
 */
dequeue :: !(Queue a) -> (!?a,!Queue a)

//* Convert a queue to a list.
toList :: !(Queue a) -> [a]
