import 'package:flutter/material.dart';
import 'package:linked_scroll_controller/linked_scroll_controller.dart';
import 'package:web_socket_channel/web_socket_channel.dart';
import 'package:web_socket_channel/status.dart' as status;
import 'package:provider/provider.dart';

DocumentCache? documentCache;

void main() async {
  runApp(const MyApp());
}

class DocumentCache extends ChangeNotifier {
  final _channel = WebSocketChannel.connect(Uri.parse('ws://localhost:13307'));
  String value = "...";

  DocumentCache() {
    _channel.stream.listen((event) {
      value = event.toString();
      notifyListeners();
    });
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
    documentCache?._channel.sink.close(status.normalClosure);
    documentCache = null;
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    ThemeData theme = Theme.of(context);
    return ChangeNotifierProvider(
      create: ((context) {
        documentCache = DocumentCache();
        return documentCache;
      }),
      child: Scaffold(
        backgroundColor: theme.scaffoldBackgroundColor,
        body: CustomScrollView(
          slivers: <Widget>[
            SliverAppBar(
              pinned: true,
              backgroundColor: theme.secondaryHeaderColor,
              centerTitle: true,
              expandedHeight: 75.0,
              flexibleSpace: FlexibleSpaceBar(
                title: Text('Document', style: theme.textTheme.headlineMedium),
              ),
            ),
            SliverFixedExtentList(
              itemExtent: 50.0,
              delegate: SliverChildBuilderDelegate(
                (BuildContext context, int row) {
                  return MeshaRow(row, _controllerGroup.addAndGet());
                },
              ),
            ),
          ],
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
  @override
  void dispose() {
    widget.controller.dispose();
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    return CustomScrollView(
      key: Key('Row-${widget.index}'),
      scrollDirection: Axis.horizontal,
      controller: widget.controller,
      slivers: <Widget>[
        SliverList(
          delegate: SliverChildBuilderDelegate(
            (BuildContext context, int col) {
              return MeshaCell('(${widget.index}, $col)', width: 100.0);
            },
          ),
        ),
      ],
    );
  }
}

class MeshaCell extends StatelessWidget {
  const MeshaCell(this.text, {super.key, this.width});
  final double? width;
  final String text;
  @override
  Widget build(BuildContext context) {
    return Consumer<DocumentCache>(
      builder: (context, cache, child) {
        return Container(
          alignment: Alignment.center,
          width: width,
          child: Text(documentCache != null ? documentCache!.value : ''),
        );
      },
    );
  }
}
