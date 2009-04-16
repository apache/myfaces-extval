/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */
package org.apache.myfaces.extensions.validator.test.el;

import org.apache.myfaces.extensions.validator.test.AbstractExValViewControllerTestCase;
import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;
import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * @author Gerhard Petracek
 * @since 1.x.2
 */
public class ValueBindingExpressionTestCase extends AbstractExValViewControllerTestCase
{
    public ValueBindingExpressionTestCase(String name)
    {
        super(name);
    }

    public static Test suite()
    {
        return new TestSuite(ValueBindingExpressionTestCase.class);
    }

    @Override
    protected void setUp() throws Exception
    {
        super.setUp();
    }

    @Override
    protected void tearDown() throws Exception
    {
        super.tearDown();
    }

    public void testStandardSyntax() throws Exception
    {
        ValueBindingExpression valueBindingExpression = new ValueBindingExpression("#{bean1.property1}");

        assertEquals(valueBindingExpression.getExpressionString(), "#{bean1.property1}");
        assertEquals(valueBindingExpression.getBaseExpression().getExpressionString(), "#{bean1}");
        assertEquals(valueBindingExpression.getProperty(), "property1");

        valueBindingExpression = new ValueBindingExpression("#{bean1['property1']}");

        assertEquals(valueBindingExpression.getExpressionString(), "#{bean1['property1']}");
        assertEquals(valueBindingExpression.getBaseExpression().getExpressionString(), "#{bean1}");
        assertEquals(valueBindingExpression.getProperty(), "property1");

        valueBindingExpression = new ValueBindingExpression("#{bean1['bean2'].property1}");

        assertEquals(valueBindingExpression.getExpressionString(), "#{bean1['bean2'].property1}");
        assertEquals(valueBindingExpression.getBaseExpression().getExpressionString(), "#{bean1['bean2']}");
        assertEquals(valueBindingExpression.getBaseExpression().getBaseExpression().getExpressionString(), "#{bean1}");
        assertEquals(valueBindingExpression.getProperty(), "property1");
    }

    public void testStandardSyntaxReplaceProperty() throws Exception
    {
        ValueBindingExpression valueBindingExpression = new ValueBindingExpression("#{bean1.property1}");

        valueBindingExpression = ValueBindingExpression.replaceProperty(valueBindingExpression, "property2");

        assertEquals(valueBindingExpression.getExpressionString(), "#{bean1.property2}");
        assertEquals(valueBindingExpression.getBaseExpression().getExpressionString(), "#{bean1}");
        assertEquals(valueBindingExpression.getProperty(), "property2");

        valueBindingExpression = new ValueBindingExpression("#{bean1['property1']}");

        valueBindingExpression = ValueBindingExpression.replaceProperty(valueBindingExpression, "property2");

        //TODO restore original syntax
        assertEquals(valueBindingExpression.getExpressionString(), "#{bean1.property2}");
        assertEquals(valueBindingExpression.getBaseExpression().getExpressionString(), "#{bean1}");
        assertEquals(valueBindingExpression.getProperty(), "property2");

        valueBindingExpression = new ValueBindingExpression("#{bean1['bean2'].property1}");

        valueBindingExpression = ValueBindingExpression.replaceProperty(valueBindingExpression, "property2");

        assertEquals(valueBindingExpression.getExpressionString(), "#{bean1['bean2'].property2}");
        assertEquals(valueBindingExpression.getBaseExpression().getExpressionString(), "#{bean1['bean2']}");
        assertEquals(valueBindingExpression.getBaseExpression().getBaseExpression().getExpressionString(), "#{bean1}");
        assertEquals(valueBindingExpression.getProperty(), "property2");
    }

    public void testStandardSyntaxAddProperty() throws Exception
    {
        ValueBindingExpression valueBindingExpression = new ValueBindingExpression("#{bean1.bean2}");

        valueBindingExpression = ValueBindingExpression.addProperty(valueBindingExpression, "property1");

        assertEquals(valueBindingExpression.getExpressionString(), "#{bean1.bean2.property1}");
        assertEquals(valueBindingExpression.getBaseExpression().getExpressionString(), "#{bean1.bean2}");
        assertEquals(valueBindingExpression.getProperty(), "property1");

        valueBindingExpression = new ValueBindingExpression("#{bean1['bean2']}");

        valueBindingExpression = ValueBindingExpression.addProperty(valueBindingExpression, "property1");

        assertEquals(valueBindingExpression.getExpressionString(), "#{bean1['bean2'].property1}");
        assertEquals(valueBindingExpression.getBaseExpression().getExpressionString(), "#{bean1['bean2']}");
        assertEquals(valueBindingExpression.getProperty(), "property1");

        valueBindingExpression = new ValueBindingExpression("#{bean1['bean2'].bean3}");

        valueBindingExpression = ValueBindingExpression.addProperty(valueBindingExpression, "property1");

        assertEquals(valueBindingExpression.getExpressionString(), "#{bean1['bean2'].bean3.property1}");
        assertEquals(valueBindingExpression.getBaseExpression().getExpressionString(), "#{bean1['bean2'].bean3}");
        assertEquals(valueBindingExpression
                .getBaseExpression().getBaseExpression().getExpressionString(), "#{bean1['bean2']}");
        assertEquals(valueBindingExpression.getProperty(), "property1");
    }

    public void testFaceletsCustomComponentSyntax() throws Exception
    {
        ValueBindingExpression valueBindingExpression = new ValueBindingExpression("#{entity[fieldName]}");

        assertEquals(valueBindingExpression.getExpressionString(), "#{entity[fieldName]}");
        assertEquals(valueBindingExpression.getBaseExpression().getExpressionString(), "#{entity}");
        assertEquals(valueBindingExpression.getProperty(), "fieldName");
    }

    public void testFaceletsCustomComponentSyntaxReplaceProperty() throws Exception
    {
        ValueBindingExpression valueBindingExpression = new ValueBindingExpression("#{entity[fieldName]}");

        valueBindingExpression = ValueBindingExpression.replaceProperty(valueBindingExpression, "newFieldName");

        //TODO restore original syntax
        assertEquals(valueBindingExpression.getExpressionString(), "#{entity.newFieldName}");
        assertEquals(valueBindingExpression.getBaseExpression().getExpressionString(), "#{entity}");
        assertEquals(valueBindingExpression.getProperty(), "newFieldName");
    }

    public void testComplexMapSyntax() throws Exception
    {
        ValueBindingExpression valueBindingExpression
                = new ValueBindingExpression("#{bean1[bean2[bean3['key1']]].property1}");

        //TODO
        //assertEquals(valueBindingExpression.getExpressionString(), "#{bean1[bean2[bean3['key1']]].property1}");
        assertEquals(valueBindingExpression.getProperty(), "property1");
    }

}
