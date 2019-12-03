// Copyright 2013 Julien Schmidt. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be found
// in the LICENSE file.

package httprouter

import (
	"strings"
	"unicode"
	"unicode/utf8"
)

func min(a, b int) int {
	if a <= b {
		return a
	}
	return b
}

func longestCommonPrefix(a, b string) int {
	i := 0
	max := min(len(a), len(b))
	for i < max && a[i] == b[i] {
		i++
	}
	return i
}

// Search for a wildcard segment and check the name for invalid characters.
// Returns -1 as index, if no wildcard was found.
// @param: wilcard  string, 匹配到的变量明, 比如 ":name"
// @param: i int, 匹配到 ":" 或者 "*" 的起始位置
// @param: valid bool, path 是否合法
func findWildcard(path string) (wilcard string, i int, valid bool) {
	// Find start
	for start, c := range []byte(path) {
		// A wildcard starts with ':' (param) or '*' (catch-all)
		// 匹配 : 和 * 开始的字符串,表示通配符的开始
		if c != ':' && c != '*' {
			continue
		}

		// Find end and check for invalid characters
		valid = true
		for end, c := range []byte(path[start+1:]) {
			switch c {
			// 匹配到通配符结束的位置，直接返回
			// 比如 "/:name/:xxx" 返回: ":name"
			// 比如 "/:/" 返回: ":"
			case '/':
				return path[start : start+1+end], start, valid
			case ':', '*': // 如果再次匹配到通配符的开始，表示两个通配符中间没有分段，非法
				valid = false
			}
		}
		// 执行到这里表示没有匹配到 '/', 到了path 的结尾
		// 比如: "/:name" 直接返回到结尾的字符串 ":name"
		// 比如: "/:" 返回 ":"
		return path[start:], start, valid
	}
	// 没有匹配到任何通配符，返回空
	return "", -1, false
}

func countParams(path string) uint16 {
	var n uint
	for i := range []byte(path) {
		switch path[i] {
		case ':', '*':
			n++
		}
	}
	return uint16(n)
}

type nodeType uint8

const (
	static nodeType = iota // default
	root
	param
	catchAll
)

type node struct {
	path      string   // 对应 URL
	indices   string   // 索引, 每个子节点的首字母，快速匹配
	wildChild bool     // 是否有动态匹配的子节点
	nType     nodeType // 节点类型
	priority  uint32   // 权重
	children  []*node  // 子节点
	handle    Handle   // 对应的 handle
}

// Increments priority of the given child and reorders if necessary
// 根据添加次数计算权重
func (n *node) incrementChildPrio(pos int) int {
	cs := n.children
	cs[pos].priority++
	prio := cs[pos].priority

	// Adjust position (move to front)
	newPos := pos
	for ; newPos > 0 && cs[newPos-1].priority < prio; newPos-- {
		// Swap node positions
		cs[newPos-1], cs[newPos] = cs[newPos], cs[newPos-1]

	}

	// Build new index char string
	if newPos != pos {
		n.indices = n.indices[:newPos] + // Unchanged prefix, might be empty
			n.indices[pos:pos+1] + // The index char we move
			n.indices[newPos:pos] + n.indices[pos+1:] // Rest without char at 'pos'
	}

	return newPos
}

// addRoute adds a node with the given handle to the path.
// Not concurrency-safe!
func (n *node) addRoute(path string, handle Handle) {
	fullPath := path
	n.priority++

	// Empty tree
	if len(n.path) == 0 && len(n.indices) == 0 {
		n.insertChild(path, fullPath, handle)
		n.nType = root
		return
	}

walk:
	for {
		// Find the longest common prefix.
		// This also implies that the common prefix contains no ':' or '*'
		// since the existing key can't contain those chars.
		// 最长前缀匹配
		// 不包含模糊匹配字符串
		i := longestCommonPrefix(path, n.path)

		// Split edge
		// i < len(n.path) 证明前缀长度  < n.path 长度，
		// 需要把原来的节点从最长匹配的位置拆分
		// 前缀存储在原节点，后面的新产生一个节点，作为子节点
		if i < len(n.path) {
			child := node{
				path:      n.path[i:], // 匹配位置后面的字符
				wildChild: n.wildChild,
				nType:     static,
				indices:   n.indices,
				children:  n.children,
				handle:    n.handle,
				priority:  n.priority - 1,
			}

			n.children = []*node{&child}
			// []byte for proper unicode char conversion, see #65
			n.indices = string([]byte{n.path[i]}) // 子节点首字符作为索引
			n.path = path[:i]                     // 修改当前节点的 path 为最长匹配字符串
			n.handle = nil
			n.wildChild = false
		}

		// Make new node a child of this node
		// i < len(path) 证明新插入的路由有一部分字符串匹配不到，需要新插入一个包含此信息的节点
		if i < len(path) {
			path = path[i:]

			if n.wildChild {
				n = n.children[0]
				n.priority++

				// Check if the wildcard matches
				// 对于子节点是变量，同样前缀下只有一个变量，如果存在多个变量名就报错
				// 唯一的不同是多了一个 '/', 其它的要一致
				if len(path) >= len(n.path) && n.path == path[:len(n.path)] &&
					// Adding a child to a catchAll is not possible
					n.nType != catchAll &&
					// Check for longer wildcard, e.g. :name and :names
					(len(n.path) >= len(path) || path[len(n.path)] == '/') {
					continue walk
				} else {
					// Wildcard conflict
					pathSeg := path
					if n.nType != catchAll {
						pathSeg = strings.SplitN(pathSeg, "/", 2)[0]
					}
					prefix := fullPath[:strings.Index(fullPath, pathSeg)] + n.path
					panic("'" + pathSeg +
						"' in new path '" + fullPath +
						"' conflicts with existing wildcard '" + n.path +
						"' in existing prefix '" + prefix +
						"'")
				}
			}

			idxc := path[0]

			// '/' after param
			// 1. 当前节点是变量;
			// 2. 未匹配到的第一个字符串是 '/';
			// 3. 子节点只有一个
			// 满足三个条件，继续跟子节点比较
			if n.nType == param && idxc == '/' && len(n.children) == 1 {
				n = n.children[0]
				n.priority++
				continue walk
			}

			// Check if a child with the next path byte exists
			// 找到索引，跟对应的子节点比较
			for i, c := range []byte(n.indices) {
				if c == idxc {
					i = n.incrementChildPrio(i)
					n = n.children[i]
					continue walk
				}
			}

			// Otherwise insert it
			// 增加新的子节点
			if idxc != ':' && idxc != '*' {
				// []byte for proper unicode char conversion, see #65
				n.indices += string([]byte{idxc}) // 增加索引
				child := &node{}
				n.children = append(n.children, child)
				n.incrementChildPrio(len(n.indices) - 1) // 重新排序计算
				n = child
			}
			n.insertChild(path, fullPath, handle) // 对当前子节点继续执行插入，直到叶子节点
			return
		}

		// Otherwise add handle to current node
		if n.handle != nil {
			panic("a handle is already registered for path '" + fullPath + "'")
		}
		n.handle = handle
		return
	}
}

func (n *node) insertChild(path, fullPath string, handle Handle) {
	for {
		// Find prefix until first wildcard
		wildcard, i, valid := findWildcard(path) //  返回匹配通配符的字符, 起始位置，是否存在
		if i < 0 {                               // No wilcard found
			break
		}

		// The wildcard name must not contain ':' and '*'
		if !valid { // 非法的通配符写法
			panic("only one wildcard per path segment is allowed, has: '" +
				wildcard + "' in path '" + fullPath + "'")
		}

		// Check if the wildcard has a name
		// 通配符必须又有一个名字
		if len(wildcard) < 2 {
			panic("wildcards must be named with a non-empty name in path '" + fullPath + "'")
		}

		// Check if this node has existing children which would be
		// unreachable if we insert the wildcard here
		// 当前节点不应该有子节点，如果子节点证明重复定义了路由规则
		if len(n.children) > 0 {
			panic("wildcard segment '" + wildcard +
				"' conflicts with existing children in path '" + fullPath + "'")
		}

		// 变量通配符
		if wildcard[0] == ':' { // param
			if i > 0 {
				// Insert prefix before the current wildcard
				n.path = path[:i] // 当前节点 path 为匹配的前缀
				path = path[i:]   // 通配符后的重新赋值
			}

			// 通配符匹配到的字符作为子节点加入当前节点
			n.wildChild = true
			child := &node{
				nType: param,    // 节点类型为: 参数
				path:  wildcard, // 参数名，例如 ":name"
			}
			n.children = []*node{child} // 插入新建的子节点
			n = child                   // n 重新赋值为子节点，继续往下插入子节点
			n.priority++

			// If the path doesn't end with the wildcard, then there
			// will be another non-wildcard subpath starting with '/'
			// 走到这里证明还有其他的变量
			// 比如: "/:name/:id"， 下一个通配符: ":id"
			// ":name"
			//	└── "/"
			//	     └── ":id"
			//
			// 继续循环查找
			if len(wildcard) < len(path) { // 表示还有 path 需要匹配
				path = path[len(wildcard):] // 为匹配的字符串重新赋值到 path
				child := &node{
					priority: 1,
				}
				n.children = []*node{child} // 当前节点插入一个子节点 child
				n = child                   // n 重新赋值为子节点，继续往下插入节点
				continue
			}

			// Otherwise we're done. Insert the handle in the new leaf
			// 走到当前 path 遍历完成
			// 当前的 n 指向的是叶子节点
			// 给这个叶子节点绑定 handle
			n.handle = handle
			return

		} else { // catchAll 所有通配符: "*"
			if i+len(wildcard) != len(path) {
				panic("catch-all routes are only allowed at the end of the path in path '" + fullPath + "'")
			}

			if len(n.path) > 0 && n.path[len(n.path)-1] == '/' {
				panic("catch-all conflicts with existing handle for the path segment root in path '" + fullPath + "'")
			}

			// Currently fixed width 1 for '/'
			i--
			if path[i] != '/' {
				panic("no / before catch-all in path '" + fullPath + "'")
			}

			n.path = path[:i] // 前缀赋值

			// First node: catchAll node with empty path
			// 创建一个子节点，放入当前节点中
			child := &node{
				wildChild: true,
				nType:     catchAll,
			}
			n.children = []*node{child}
			n.indices = string('/')
			n = child
			n.priority++

			// Second node: node holding the variable
			// 创建子节点，匹配所有，并绑定 handle
			child = &node{
				path:     path[i:],
				nType:    catchAll,
				handle:   handle,
				priority: 1,
			}
			n.children = []*node{child}

			return
		}
	}

	// If no wildcard was found, simple insert the path and handle
	// 没有动态路由，直接绑定 handle
	n.path = path
	n.handle = handle
}

// Returns the handle registered with the given path (key). The values of
// wildcards are saved to a map.
// If no handle can be found, a TSR (trailing slash redirect) recommendation is
// made if a handle exists with an extra (without the) trailing slash for the
// given path.
func (n *node) getValue(path string, params func() *Params) (handle Handle, ps *Params, tsr bool) {
walk: // Outer loop for walking the tree
	for {
		prefix := n.path
		if len(path) > len(prefix) {
			if path[:len(prefix)] == prefix { // 匹配当前节点的 path 前缀
				path = path[len(prefix):]

				// If this node does not have a wildcard (param or catchAll)
				// child, we can just look up the next child node and continue
				// to walk down the tree
				// 如果子节点不是变量，则通过首字母索引来查找对应的子节点，继续匹配
				if !n.wildChild {
					idxc := path[0]
					for i, c := range []byte(n.indices) {
						if c == idxc {
							n = n.children[i]
							prefix = n.path
							continue walk
						}
					}

					// Nothing found.
					// We can recommend to redirect to the same URL without a
					// trailing slash if a leaf exists for that path.
					tsr = (path == "/" && n.handle != nil)
					return

				}

				// Handle wildcard child
				// 变量子节点只有一个
				n = n.children[0]
				switch n.nType {
				case param:
					// Find param end (either '/' or path end)
					end := 0
					for end < len(path) && path[end] != '/' {
						end++
					}

					// Save param value
					if params != nil {
						if ps == nil {
							ps = params()
						}
						// Expand slice within preallocated capacity
						i := len(*ps)
						*ps = (*ps)[:i+1]
						(*ps)[i] = Param{
							Key:   n.path[1:], // 比如 ":name" 的 key 是 name
							Value: path[:end], // value 使 path 中的字符串
						}
					}

					// We need to go deeper!
					// 如果path还没完，继续往下找
					if end < len(path) {
						if len(n.children) > 0 {
							path = path[end:]
							n = n.children[0]
							prefix = n.path
							continue walk
						}

						// ... but we can't
						tsr = (len(path) == end+1) // 到叶子节点，判断是否有反斜杠
						return
					}

					// url 匹配完了
					if handle = n.handle; handle != nil { // 找到响应的处理函数，返回
						return
					} else if len(n.children) == 1 { // 是否存在于 "/"结尾的叶子节点中
						// No handle found. Check if a handle for this path + a
						// trailing slash exists for TSR recommendation
						n = n.children[0]
						tsr = (n.path == "/" && n.handle != nil)
					}

					return

				case catchAll:
					// Save param value
					if params != nil {
						if ps == nil {
							ps = params()
						}
						// Expand slice within preallocated capacity
						i := len(*ps)
						*ps = (*ps)[:i+1]
						(*ps)[i] = Param{
							Key:   n.path[2:],
							Value: path,
						}
					}

					handle = n.handle
					return

				default:
					panic("invalid node type")
				}
			}
		} else if path == prefix {
			// We should have reached the node containing the handle.
			// Check if this node has a handle registered.
			if handle = n.handle; handle != nil {
				return
			}

			// If there is no handle for this route, but this route has a
			// wildcard child, there must be a handle for this path with an
			// additional trailing slash
			if path == "/" && n.wildChild && n.nType != root {
				tsr = true
				return
			}

			// No handle found. Check if a handle for this path + a
			// trailing slash exists for trailing slash recommendation
			for i, c := range []byte(n.indices) {
				if c == '/' {
					n = n.children[i]
					tsr = (len(n.path) == 1 && n.handle != nil) ||
						(n.nType == catchAll && n.children[0].handle != nil)
					return
				}
			}
			return
		}

		// Nothing found. We can recommend to redirect to the same URL with an
		// extra trailing slash if a leaf exists for that path
		tsr = (path == "/") ||
			(len(prefix) == len(path)+1 && prefix[len(path)] == '/' &&
				path == prefix[:len(prefix)-1] && n.handle != nil)
		return
	}
}

// Makes a case-insensitive lookup of the given path and tries to find a handler.
// It can optionally also fix trailing slashes.
// It returns the case-corrected path and a bool indicating whether the lookup
// was successful.
func (n *node) findCaseInsensitivePath(path string, fixTrailingSlash bool) (fixedPath string, found bool) {
	ciPath := n.findCaseInsensitivePathRec(
		path,
		make([]byte, 0, len(path)+1), // Preallocate enough memory for new path
		[4]byte{},                    // Empty rune buffer
		fixTrailingSlash,
	)
	return string(ciPath), ciPath != nil
}

// Shift bytes in array by n bytes left
func shiftNRuneBytes(rb [4]byte, n int) [4]byte {
	switch n {
	case 0:
		return rb
	case 1:
		return [4]byte{rb[1], rb[2], rb[3], 0}
	case 2:
		return [4]byte{rb[2], rb[3]}
	case 3:
		return [4]byte{rb[3]}
	default:
		return [4]byte{}
	}
}

// Recursive case-insensitive lookup function used by n.findCaseInsensitivePath
func (n *node) findCaseInsensitivePathRec(path string, ciPath []byte, rb [4]byte, fixTrailingSlash bool) []byte {
	npLen := len(n.path)

walk: // Outer loop for walking the tree
	for len(path) >= npLen && (npLen == 0 || strings.EqualFold(path[1:npLen], n.path[1:])) {
		// Add common prefix to result
		oldPath := path
		path = path[npLen:]
		ciPath = append(ciPath, n.path...)

		if len(path) > 0 {
			// If this node does not have a wildcard (param or catchAll) child,
			// we can just look up the next child node and continue to walk down
			// the tree
			if !n.wildChild {
				// Skip rune bytes already processed
				rb = shiftNRuneBytes(rb, npLen)

				if rb[0] != 0 {
					// Old rune not finished
					idxc := rb[0]
					for i, c := range []byte(n.indices) {
						if c == idxc {
							// continue with child node
							n = n.children[i]
							npLen = len(n.path)
							continue walk
						}
					}
				} else {
					// Process a new rune
					var rv rune

					// Find rune start.
					// Runes are up to 4 byte long,
					// -4 would definitely be another rune.
					var off int
					for max := min(npLen, 3); off < max; off++ {
						if i := npLen - off; utf8.RuneStart(oldPath[i]) {
							// read rune from cached path
							rv, _ = utf8.DecodeRuneInString(oldPath[i:])
							break
						}
					}

					// Calculate lowercase bytes of current rune
					lo := unicode.ToLower(rv)
					utf8.EncodeRune(rb[:], lo)

					// Skip already processed bytes
					rb = shiftNRuneBytes(rb, off)

					idxc := rb[0]
					for i, c := range []byte(n.indices) {
						// Lowercase matches
						if c == idxc {
							// must use a recursive approach since both the
							// uppercase byte and the lowercase byte might exist
							// as an index
							if out := n.children[i].findCaseInsensitivePathRec(
								path, ciPath, rb, fixTrailingSlash,
							); out != nil {
								return out
							}
							break
						}
					}

					// If we found no match, the same for the uppercase rune,
					// if it differs
					if up := unicode.ToUpper(rv); up != lo {
						utf8.EncodeRune(rb[:], up)
						rb = shiftNRuneBytes(rb, off)

						idxc := rb[0]
						for i, c := range []byte(n.indices) {
							// Uppercase matches
							if c == idxc {
								// Continue with child node
								n = n.children[i]
								npLen = len(n.path)
								continue walk
							}
						}
					}
				}

				// Nothing found. We can recommend to redirect to the same URL
				// without a trailing slash if a leaf exists for that path
				if fixTrailingSlash && path == "/" && n.handle != nil {
					return ciPath
				}
				return nil
			}

			n = n.children[0]
			switch n.nType {
			case param:
				// Find param end (either '/' or path end)
				end := 0
				for end < len(path) && path[end] != '/' {
					end++
				}

				// Add param value to case insensitive path
				ciPath = append(ciPath, path[:end]...)

				// We need to go deeper!
				if end < len(path) {
					if len(n.children) > 0 {
						// Continue with child node
						n = n.children[0]
						npLen = len(n.path)
						path = path[end:]
						continue
					}

					// ... but we can't
					if fixTrailingSlash && len(path) == end+1 {
						return ciPath
					}
					return nil
				}

				if n.handle != nil {
					return ciPath
				} else if fixTrailingSlash && len(n.children) == 1 {
					// No handle found. Check if a handle for this path + a
					// trailing slash exists
					n = n.children[0]
					if n.path == "/" && n.handle != nil {
						return append(ciPath, '/')
					}
				}
				return nil

			case catchAll:
				return append(ciPath, path...)

			default:
				panic("invalid node type")
			}
		} else {
			// We should have reached the node containing the handle.
			// Check if this node has a handle registered.
			if n.handle != nil {
				return ciPath
			}

			// No handle found.
			// Try to fix the path by adding a trailing slash
			if fixTrailingSlash {
				for i, c := range []byte(n.indices) {
					if c == '/' {
						n = n.children[i]
						if (len(n.path) == 1 && n.handle != nil) ||
							(n.nType == catchAll && n.children[0].handle != nil) {
							return append(ciPath, '/')
						}
						return nil
					}
				}
			}
			return nil
		}
	}

	// Nothing found.
	// Try to fix the path by adding / removing a trailing slash
	if fixTrailingSlash {
		if path == "/" {
			return ciPath
		}
		if len(path)+1 == npLen && n.path[len(path)] == '/' &&
			strings.EqualFold(path[1:], n.path[1:len(path)]) && n.handle != nil {
			return append(ciPath, n.path...)
		}
	}
	return nil
}
