/**
 * implement a container like std::linked_hashmap
 */
#ifndef SJTU_LINKEDHASHMAP_HPP
#define SJTU_LINKEDHASHMAP_HPP

// only for std::equal_to<T> and std::hash<T>
#include <functional>
#include <cstddef>
#include "utility.hpp"
#include "exceptions.hpp"

namespace sjtu {
    /**
     * In linked_hashmap, iteration ordering is differ from map,
     * which is the order in which keys were inserted into the map.
     * You should maintain a doubly-linked list running through all
     * of its entries to keep the correct iteration order.
     *
     * Note that insertion order is not affected if a key is re-inserted
     * into the map.
     */

template<
	class Key,
	class T,
	class Hash = std::hash<Key>,
	class Equal = std::equal_to<Key>
> class linked_hashmap {
public:
	/**
	 * the internal type of data.
	 * it should have a default constructor, a copy constructor.
	 * You can use sjtu::linked_hashmap as value_type by typedef.
	 */
	typedef pair<const Key, T> value_type;

private:
	// Node structure for doubly linked list
	struct Node {
		value_type* data;
		Node* prev;
		Node* next;

		Node(const value_type& val) : data(new value_type(val)), prev(nullptr), next(nullptr) {}
		Node() : data(nullptr), prev(nullptr), next(nullptr) {} // Only for dummy nodes
		~Node() { delete data; }
	};

	// Hash table entry
	struct HashEntry {
		Node* node;
		HashEntry* next;

		HashEntry() : node(nullptr), next(nullptr) {}
		HashEntry(Node* n) : node(n), next(nullptr) {}
	};

	// Data members
	HashEntry** table;
	size_t table_size;
	size_t element_count;
	Node* head;
	Node* tail;
	Hash hash_func;
	Equal equal_func;

	// Constants for rehashing
	static const double LOAD_FACTOR;
	static const size_t DEFAULT_CAPACITY = 32; // Larger initial capacity

	// Private helper methods
	size_t hash_key(const Key& key) const {
		return hash_func(key) % table_size;
	}

	void rehash() {
		size_t new_size = table_size * 2;
		HashEntry** new_table = new HashEntry*[new_size];
		for (size_t i = 0; i < new_size; ++i) {
			new_table[i] = nullptr;
		}

		// Rehash all existing elements - move existing HashEntry objects instead of creating new ones
		for (size_t i = 0; i < table_size; ++i) {
			HashEntry* entry = table[i];
			while (entry) {
				HashEntry* next_entry = entry->next;
				size_t new_index = hash_func(entry->node->data->first) % new_size;

				// Move to new table
				entry->next = new_table[new_index];
				new_table[new_index] = entry;

				entry = next_entry;
			}
		}

		delete[] table;
		table = new_table;
		table_size = new_size;
	}

public:
	/**
	 * see BidirectionalIterator at CppReference for help.
	 *
	 * if there is anything wrong throw invalid_iterator.
	 *     like it = linked_hashmap.begin(); --it;
	 *       or it = linked_hashmap.end(); ++end();
	 */
	class const_iterator;
	class iterator {
	private:
		/**
		 * TODO add data members
		 *   just add whatever you want.
		 */
		Node* current;
		linked_hashmap* container;
		bool is_end;

	public:
		// The following code is written for the C++ type_traits library.
		// Type traits is a C++ feature for describing certain properties of a type.
		// For instance, for an iterator, iterator::value_type is the type that the
		// iterator points to.
		// STL algorithms and containers may use these type_traits (e.g. the following
		// typedef) to work properly.
		// See these websites for more information:
		// https://en.cppreference.com/w/cpp/header/type_traits
		// About value_type: https://blog.csdn.net/u014299153/article/details/72419713
		// About iterator_category: https://en.cppreference.com/w/cpp/iterator
		using difference_type = std::ptrdiff_t;
		using value_type = typename linked_hashmap::value_type;
		using pointer = value_type*;
		using reference = value_type&;
		using iterator_category = std::output_iterator_tag;


		iterator() : current(nullptr), container(nullptr), is_end(false) {}
		iterator(Node* node, linked_hashmap* cont, bool end = false) : current(node), container(cont), is_end(end) {}
		iterator(const iterator &other) : current(other.current), container(other.container), is_end(other.is_end) {}

		/**
		 * TODO iter++
		 */
		iterator operator++(int) {
			if (is_end || current == container->tail) {
				throw invalid_iterator();
			}
			iterator temp = *this;
			current = current->next;
			if (current == container->tail) {
				is_end = true;
			}
			return temp;
		}

		/**
		 * TODO ++iter
		 */
		iterator & operator++() {
			if (is_end || current == container->tail) {
				throw invalid_iterator();
			}
			current = current->next;
			if (current == container->tail) {
				is_end = true;
			}
			return *this;
		}

		/**
		 * TODO iter--
		 */
		iterator operator--(int) {
			if (is_end && current == container->tail) {
				if (container->head->next == container->tail) {
					// Empty map - can't decrement end()
					throw invalid_iterator();
				}
				iterator temp = iterator(container->tail->prev, container, false);
				is_end = false;
				current = container->tail->prev;
				return temp;
			}
			if (current == container->head) {
				throw invalid_iterator();
			}
			iterator temp = *this;
			current = current->prev;
			return temp;
		}

		/**
		 * TODO --iter
		 */
		iterator & operator--() {
			if (is_end && current == container->tail) {
				if (container->head->next == container->tail) {
					// Empty map - can't decrement end()
					throw invalid_iterator();
				}
				current = container->tail->prev;
				is_end = false;
				return *this;
			}
			if (current == container->head) {
				throw invalid_iterator();
			}
			current = current->prev;
			return *this;
		}

		/**
		 * a operator to check whether two iterators are same (pointing to the same memory).
		 */
		value_type & operator*() const {
			if (is_end || current == container->head || current == container->tail) {
				throw invalid_iterator();
			}
			return *(current->data);
		}

		bool operator==(const iterator &rhs) const {
			return current == rhs.current && container == rhs.container && is_end == rhs.is_end;
		}

		bool operator==(const const_iterator &rhs) const {
			return current == rhs.current && container == rhs.container && is_end == rhs.is_end;
		}

		/**
		 * some other operator for iterator.
		 */
		bool operator!=(const iterator &rhs) const {
			return !(*this == rhs);
		}

		bool operator!=(const const_iterator &rhs) const {
			return !(*this == rhs);
		}

		/**
		 * for the support of it->first.
		 * See <http://kelvinh.github.io/blog/2013/11/20/overloading-of-member-access-operator-dash-greater-than-symbol-in-cpp/> for help.
		 */
		value_type* operator->() const {
			if (is_end || current == container->head || current == container->tail) {
				throw invalid_iterator();
			}
			return current->data;
		}

		friend class linked_hashmap;
		friend class const_iterator;
	};

	class const_iterator {
		// it should has similar member method as iterator.
		//  and it should be able to construct from an iterator.
		private:
			// data members.
			const Node* current;
			const linked_hashmap* container;
			bool is_end;

		public:
			const_iterator() : current(nullptr), container(nullptr), is_end(false) {}
			const_iterator(const Node* node, const linked_hashmap* cont, bool end = false) : current(node), container(cont), is_end(end) {}
			const_iterator(const const_iterator &other) : current(other.current), container(other.container), is_end(other.is_end) {}
			const_iterator(const iterator &other) : current(other.current), container(other.container), is_end(other.is_end) {}

			// And other methods in iterator.
			const_iterator operator++(int) {
				if (is_end || current == container->tail) {
					throw invalid_iterator();
				}
				const_iterator temp = *this;
				current = current->next;
				if (current == container->tail) {
					is_end = true;
				}
				return temp;
			}

			const_iterator & operator++() {
				if (is_end || current == container->tail) {
					throw invalid_iterator();
				}
				current = current->next;
				if (current == container->tail) {
					is_end = true;
				}
				return *this;
			}

			const_iterator operator--(int) {
				if (is_end && current == container->tail) {
					if (container->head->next == container->tail) {
						// Empty map - can't decrement end()
						throw invalid_iterator();
					}
					const_iterator temp = const_iterator(container->tail->prev, container, false);
					is_end = false;
					current = container->tail->prev;
					return temp;
				}
				if (current == container->head) {
					throw invalid_iterator();
				}
				const_iterator temp = *this;
				current = current->prev;
				return temp;
			}

			const_iterator & operator--() {
				if (is_end && current == container->tail) {
					if (container->head->next == container->tail) {
						// Empty map - can't decrement end()
						throw invalid_iterator();
					}
					current = container->tail->prev;
					is_end = false;
					return *this;
				}
				if (current == container->head) {
					throw invalid_iterator();
				}
				current = current->prev;
				return *this;
			}

			const value_type & operator*() const {
				if (is_end || current == container->head || current == container->tail) {
					throw invalid_iterator();
				}
				return *(current->data);
			}

			bool operator==(const iterator &rhs) const {
				return current == rhs.current && container == rhs.container && is_end == rhs.is_end;
			}

			bool operator==(const const_iterator &rhs) const {
				return current == rhs.current && container == rhs.container && is_end == rhs.is_end;
			}

			bool operator!=(const iterator &rhs) const {
				return !(*this == rhs);
			}

			bool operator!=(const const_iterator &rhs) const {
				return !(*this == rhs);
			}

			const value_type* operator->() const {
				if (is_end || current == container->head || current == container->tail) {
					throw invalid_iterator();
				}
				return current->data;
			}

			friend class linked_hashmap;
	};

	/**
	 * TODO two constructors
	 */
	linked_hashmap() : table_size(DEFAULT_CAPACITY), element_count(0) {
		table = new HashEntry*[table_size];
		for (size_t i = 0; i < table_size; ++i) {
			table[i] = nullptr;
		}
		head = new Node();
		tail = new Node();
		head->next = tail;
		tail->prev = head;
	}

	linked_hashmap(const linked_hashmap &other) : table_size(other.table_size), element_count(0), hash_func(other.hash_func), equal_func(other.equal_func) {
		table = new HashEntry*[table_size];
		for (size_t i = 0; i < table_size; ++i) {
			table[i] = nullptr;
		}
		head = new Node();
		tail = new Node();
		head->next = tail;
		tail->prev = head;

		// Copy all elements
		for (const_iterator it = other.cbegin(); it != other.cend(); ++it) {
			insert(*it);
		}
	}

	/**
	 * TODO assignment operator
	 */
	linked_hashmap & operator=(const linked_hashmap &other) {
		if (this != &other) {
			clear();

			// Delete old table and create new one with same size
			for (size_t i = 0; i < table_size; ++i) {
				HashEntry* current = table[i];
				while (current) {
					HashEntry* temp = current;
					current = current->next;
					delete temp;
				}
			}
			delete[] table;

			table_size = other.table_size;
			table = new HashEntry*[table_size];
			for (size_t i = 0; i < table_size; ++i) {
				table[i] = nullptr;
			}

			// Copy all elements
			for (const_iterator it = other.cbegin(); it != other.cend(); ++it) {
				insert(*it);
			}
		}
		return *this;
	}

	/**
	 * TODO Destructors
	 */
	~linked_hashmap() {
		clear();

		// Delete hash table
		for (size_t i = 0; i < table_size; ++i) {
			HashEntry* current = table[i];
			while (current) {
				HashEntry* temp = current;
				current = current->next;
				delete temp;
			}
		}
		delete[] table;

		// Delete dummy nodes
		delete head;
		delete tail;
	}

	/**
	 * TODO
	 * access specified element with bounds checking
	 * Returns a reference to the mapped value of the element with key equivalent to key.
	 * If no such element exists, an exception of type `index_out_of_bound'
	 */
	T & at(const Key &key) {
		iterator it = find(key);
		if (it == end()) {
			throw index_out_of_bound();
		}
		return it->second;
	}

	const T & at(const Key &key) const {
		const_iterator it = find(key);
		if (it == cend()) {
			throw index_out_of_bound();
		}
		return it->second;
	}

	/**
	 * TODO
	 * access specified element
	 * Returns a reference to the value that is mapped to a key equivalent to key,
	 *   performing an insertion if such key does not already exist.
	 */
	T & operator[](const Key &key) {
		iterator it = find(key);
		if (it == end()) {
			pair<iterator, bool> result = insert(value_type(key, T()));
			return result.first->second;
		}
		return it->second;
	}

	/**
	 * behave like at() throw index_out_of_bound if such key does not exist.
	 */
	const T & operator[](const Key &key) const {
		return at(key);
	}

	/**
	 * return a iterator to the beginning
	 */
	iterator begin() {
		if (head->next == tail) {
			return iterator(tail, this, true);
		}
		return iterator(head->next, this, false);
	}

	const_iterator cbegin() const {
		if (head->next == tail) {
			return const_iterator(tail, this, true);
		}
		return const_iterator(head->next, this, false);
	}

	/**
	 * return a iterator to the end
	 * in fact, it returns past-the-end.
	 */
	iterator end() {
		return iterator(tail, this, true);
	}

	const_iterator cend() const {
		return const_iterator(tail, this, true);
	}

	/**
	 * checks whether the container is empty
	 * return true if empty, otherwise false.
	 */
	bool empty() const {
		return element_count == 0;
	}

	/**
	 * returns the number of elements.
	 */
	size_t size() const {
		return element_count;
	}

	/**
	 * clears the contents
	 */
	void clear() {
		// Delete all nodes
		Node* current = head->next;
		while (current != tail) {
			Node* temp = current;
			current = current->next;
			delete temp;
		}
		head->next = tail;
		tail->prev = head;

		// Clear hash table
		for (size_t i = 0; i < table_size; ++i) {
			HashEntry* current = table[i];
			while (current) {
				HashEntry* temp = current;
				current = current->next;
				delete temp;
			}
			table[i] = nullptr;
		}

		element_count = 0;
	}

	/**
	 * insert an element.
	 * return a pair, the first of the pair is
	 *   the iterator to the new element (or the element that prevented the insertion),
	 *   the second one is true if insert successfully, or false.
	 */
	pair<iterator, bool> insert(const value_type &value) {
		// Check if key already exists
		iterator it = find(value.first);
		if (it != end()) {
			return pair<iterator, bool>(it, false);
		}

		// Check if rehashing is needed
		if (element_count >= table_size * LOAD_FACTOR) {
			rehash();
		}

		// Create new node
		Node* new_node = new Node(value);

		// Add to linked list
		new_node->prev = tail->prev;
		new_node->next = tail;
		tail->prev->next = new_node;
		tail->prev = new_node;

		// Add to hash table
		size_t index = hash_key(value.first);
		HashEntry* entry = new HashEntry(new_node);
		entry->next = table[index];
		table[index] = entry;

		element_count++;
		return pair<iterator, bool>(iterator(new_node, this, false), true);
	}

	/**
	 * erase the element at pos.
	 *
	 * throw if pos pointed to a bad element (pos == this->end() || pos points an element out of this)
	 */
	void erase(iterator pos) {
		if (pos.container != this || pos.is_end || pos.current == head || pos.current == tail) {
			throw invalid_iterator();
		}

		// Remove from hash table
		size_t index = hash_key(pos.current->data->first);
		HashEntry** current = &table[index];
		while (*current) {
			if ((*current)->node == pos.current) {
				HashEntry* temp = *current;
				*current = (*current)->next;
				delete temp;
				break;
			}
			current = &((*current)->next);
		}

		// Remove from linked list
		pos.current->prev->next = pos.current->next;
		pos.current->next->prev = pos.current->prev;
		delete pos.current;

		element_count--;
	}

	/**
	 * Returns the number of elements with key
	 *   that compares equivalent to the specified argument,
	 *   which is either 1 or 0
	 *     since this container does not allow duplicates.
	 */
	size_t count(const Key &key) const {
		return find(key) != cend() ? 1 : 0;
	}

	/**
	 * Finds an element with key equivalent to key.
	 * key value of the element to search for.
	 * Iterator to an element with key equivalent to key.
	 *   If no such element is found, past-the-end (see end()) iterator is returned.
	 */
	iterator find(const Key &key) {
		size_t index = hash_key(key);
		HashEntry* entry = table[index];
		while (entry) {
			if (equal_func(entry->node->data->first, key)) {
				return iterator(entry->node, this, false);
			}
			entry = entry->next;
		}
		return end();
	}

	const_iterator find(const Key &key) const {
		size_t index = hash_key(key);
		HashEntry* entry = table[index];
		while (entry) {
			if (equal_func(entry->node->data->first, key)) {
				return const_iterator(entry->node, this, false);
			}
			entry = entry->next;
		}
		return cend();
	}
};

template<class Key, class T, class Hash, class Equal>
const double linked_hashmap<Key, T, Hash, Equal>::LOAD_FACTOR = 0.8; // Higher load factor to reduce rehashing

}

#endif
