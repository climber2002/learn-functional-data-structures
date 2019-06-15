package com.functional.ch03

object Dictionary {
  type Dictionary[A] = BinTree[(String, A)]

  def insert[A](key: String, value: A, dict: Dictionary[A]): Dictionary[A] = dict match {
    case Leaf => Branch((key, value), Leaf, Leaf)
    case Branch((k, _), _, _) if k == key => sys.error(s"key ${key} already present")
    case Branch((k, v), l, r) if k < key => Branch((k, v), insert(key, value, l), r)
    case Branch((k, v), l, r) if k > key => Branch((k, v), l, insert(key, value, r))
  }

  def search[A](key: String, dict: Dictionary[A]): Option[A] = dict match {
    case Leaf => None
    case Branch((k, v), _, _) if k == key => Option(v)
    case Branch((k, _), l, _) if k < key => search(key, l)
    case Branch((k, _), _, r) if k > key => search(key, r)
  }

  def update[A](key: String, value: A, dict: Dictionary[A]): Dictionary[A] = dict match {
    case Leaf => Branch((key, value), Leaf, Leaf)
    case Branch((k, v), l, r) if (k == key) => Branch((k, value), l, r)
    case Branch((k, v), l, r) if (k > key) => Branch((k, value), update(key, value, l), r)
    case Branch((k, v), l, r) if (k < key) => Branch((k, value), l, update(key, value, r))
  }
}


