import 'dart:collection';
import 'dart:convert';
import 'dart:math';

import 'package:flutter/material.dart';
import 'package:flutter/rendering.dart';
import 'package:linked_scroll_controller/linked_scroll_controller.dart';
import 'package:provider/provider.dart';
import 'package:web_socket_channel/status.dart' as status;
import 'package:web_socket_channel/web_socket_channel.dart';

BlockCache blockCache = BlockCache();

void main() async {
  runApp(const MyApp());
}

class Block extends ChangeNotifier {
  int _id = 0;
  Object _value = "";
  bool _hasValue = false;

  Block(id) {
    _id = id;
    _hasValue = false;
  }

  @override
  void dispose() {
    super.dispose();
  }

  void update(int id, Object value) {
    assert(_id == id);
    _value = value;
    _hasValue = true;
    notifyListeners();
  }
}

class BlockCache extends ChangeNotifier {
  final _channel = WebSocketChannel.connect(Uri.parse('ws://localhost:13307'));
  final HashMap<int, Block> _cache = HashMap();

  BlockCache() {
    _channel.stream.listen((msg) {
      final m = jsonDecode(msg);
      int id = m['id'];
      Block? b = _cache[id];
      if (b == null) {
        b = Block(id);
        _cache[id] = b;
      }

      b.update(id, m['value']);
      notifyListeners();
    });
  }

  Block? getBlock(int id) {
    Block? b = _cache[id];
    if (b == null) {
      b = Block(id);
      _cache[id] = b;
    }

    if (!b._hasValue) {
      _channel.sink.add("(:operation :get-block :id $id)");
    }

    return b;
  }

  void updateBlock(int id, String value) {
    _channel.sink.add("(:operation :set-block :id $id :value \"$value\")");
  }
}

class MyApp extends StatelessWidget {
  const MyApp({super.key});

  // This widget is the root of your application.
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'Mesha',
      theme: ThemeData(
        colorScheme: ColorScheme.fromSeed(seedColor: const Color.fromARGB(255, 40, 211, 25)),
        useMaterial3: true,
      ),
      home: const MeshaDocument(),
    );
  }
}

class MeshaDocument extends StatefulWidget {
  const MeshaDocument({super.key});

  @override
  State<MeshaDocument> createState() => _MeshaDocumentState();
}

class _MeshaDocumentState extends State<MeshaDocument> {
  final LinkedScrollControllerGroup _controllerGroup = LinkedScrollControllerGroup();

  @override
  void dispose() {
    blockCache._channel.sink.close(status.normalClosure);
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    ThemeData theme = Theme.of(context);
    return ChangeNotifierProvider(
      create: ((context) {
        return blockCache;
      }),
      child: Scaffold(
        backgroundColor: theme.scaffoldBackgroundColor,
        body: const MeshaCell(
          id: 1,
          width: 500.0,
        ),
      ),
    );
  }
}

class MeshaRow extends StatefulWidget {
  const MeshaRow(this.index, this.controller, {super.key});
  final int index;
  final ScrollController controller;

  @override
  State<MeshaRow> createState() => _MeshaRowState();
}

class _MeshaRowState extends State<MeshaRow> {
  _MeshaRowState() {}

  @override
  void dispose() {
    widget.controller.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return CustomScrollView(
      key: Key('Row-$widget.index'),
      scrollDirection: Axis.horizontal,
      controller: widget.controller,
      slivers: <Widget>[
        SliverList(
          delegate: SliverChildBuilderDelegate(
            (BuildContext context, int col) {
              return const MeshaCell(id: 1, width: 100.0);
            },
          ),
        ),
      ],
    );
  }
}

class MeshaCell extends StatefulWidget {
  const MeshaCell({super.key, required this.id, this.width});
  final double? width;
  final int id;

  @override
  State<MeshaCell> createState() => _MeshaCellState();
}

class _MeshaCellState extends State<MeshaCell> {
  late double _width;
  late int _id;
  late TextEditingController _textEditingController;

  @override
  void initState() {
    super.initState();
    _width = widget.width == null ? 100.0 : widget.width!;
    _id = widget.id;
    _textEditingController = TextEditingController();
  }

  @override
  Widget build(BuildContext context) {
    return ChangeNotifierProvider(
      create: ((context) {
        return blockCache.getBlock(_id);
      }),
      child: Consumer<Block>(
        builder: (context, block, child) {
          _textEditingController.text = block._value.toString();
          return Container(
            alignment: Alignment.center,
            width: _width,
            child: TextField(
              controller: _textEditingController,
              onSubmitted: (String value) async {
                blockCache.updateBlock(block._id, value);
              },
            ),
          );
        },
      ),
    );
  }
}

class AdjustableScrollController extends ScrollController {
  AdjustableScrollController([int extraScrollSpeed = 40]) {
    super.addListener(() {
      ScrollDirection scrollDirection = super.position.userScrollDirection;
      if (scrollDirection != ScrollDirection.idle) {
        int slack = (scrollDirection == ScrollDirection.reverse) ? extraScrollSpeed : -extraScrollSpeed;
        double scrollEnd = super.offset + slack;
        scrollEnd = min(super.position.maxScrollExtent, max(super.position.minScrollExtent, scrollEnd));
        jumpTo(scrollEnd);
      }
    });
  }
}
