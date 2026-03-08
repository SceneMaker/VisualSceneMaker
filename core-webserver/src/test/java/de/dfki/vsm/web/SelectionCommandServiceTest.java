package de.dfki.vsm.web;

import de.dfki.vsm.model.sceneflow.chart.edge.GuargedEdge;
import de.dfki.vsm.model.sceneflow.chart.edge.InterruptEdge;
import de.dfki.vsm.model.sceneflow.glue.command.Expression;
import de.dfki.vsm.model.sceneflow.glue.command.expression.invocation.RandomQuery;
import de.dfki.vsm.model.sceneflow.glue.command.expression.literal.IntLiteral;
import org.junit.jupiter.api.Test;

import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;

class SelectionCommandServiceTest {

    @Test
    void copyUsesConcreteSyntaxForConditionalAndInterruptEdges() throws Exception {
        SelectionCommandService service = new SelectionCommandService();
        List<SelectionCommandService.ClipboardEdgeData> clipboard = new ArrayList<>();
        Set<String> selectedNodeIds = Set.of("target");

        Expression cCondition = new RandomQuery(new IntLiteral(3));
        GuargedEdge cedge = new GuargedEdge();
        cedge.setTargetUnid("target");
        cedge.setCondition(cCondition);

        Expression iCondition = new RandomQuery(new IntLiteral(7));
        InterruptEdge iedge = new InterruptEdge();
        iedge.setTargetUnid("target");
        iedge.setCondition(iCondition);

        Method collect = SelectionCommandService.class.getDeclaredMethod(
                "collectEdgesForClipboard",
                List.class,
                List.class,
                String.class,
                String.class,
                Set.class
        );
        collect.setAccessible(true);

        collect.invoke(service, clipboard, List.of(cedge), "source", "CEDGE", selectedNodeIds);
        collect.invoke(service, clipboard, List.of(iedge), "source", "IEDGE", selectedNodeIds);

        assertEquals(2, clipboard.size());
        assertEquals(cCondition.getConcreteSyntax(), clipboard.get(0).condition);
        assertEquals(iCondition.getConcreteSyntax(), clipboard.get(1).condition);
    }
}
