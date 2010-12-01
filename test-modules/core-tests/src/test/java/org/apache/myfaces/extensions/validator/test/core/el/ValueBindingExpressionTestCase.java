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
package org.apache.myfaces.extensions.validator.test.core.el;

import org.apache.myfaces.extensions.validator.core.el.ValueBindingExpression;
import org.apache.myfaces.extensions.validator.test.core.AbstractExValCoreTestCase;
import org.junit.Assert;
import org.junit.Test;

/**
 * @author Gerhard Petracek
 * @since 1.x.2
 */
public class ValueBindingExpressionTestCase extends AbstractExValCoreTestCase
{

    @Test
    public void testStandardSyntax() throws Exception
    {
        ValueBindingExpression valueBindingExpression = new ValueBindingExpression("#{bean1.property1}");

        Assert.assertEquals("#{bean1.property1}", valueBindingExpression.getExpressionString());
        Assert.assertEquals("#{bean1}", valueBindingExpression.getBaseExpression().getExpressionString());
        Assert.assertEquals("property1", valueBindingExpression.getProperty());

        valueBindingExpression = new ValueBindingExpression("#{bean1['property1']}");

        Assert.assertEquals("#{bean1['property1']}", valueBindingExpression.getExpressionString());
        Assert.assertEquals("#{bean1}", valueBindingExpression.getBaseExpression().getExpressionString());
        Assert.assertEquals("property1", valueBindingExpression.getProperty());

        valueBindingExpression = new ValueBindingExpression("#{bean1['bean2'].property1}");

        Assert.assertEquals("#{bean1['bean2'].property1}", valueBindingExpression.getExpressionString());
        Assert.assertEquals("#{bean1['bean2']}", valueBindingExpression.getBaseExpression().getExpressionString());
        Assert.assertEquals("#{bean1}", valueBindingExpression.getBaseExpression().getBaseExpression().getExpressionString());
        Assert.assertEquals("property1", valueBindingExpression.getProperty());
    }

    @Test
    public void testStandardSyntaxReplaceProperty() throws Exception
    {
        ValueBindingExpression valueBindingExpression = new ValueBindingExpression("#{bean1.property1}");

        valueBindingExpression = ValueBindingExpression.replaceProperty(valueBindingExpression, "property2");

        Assert.assertEquals("#{bean1.property2}", valueBindingExpression.getExpressionString());
        Assert.assertEquals("#{bean1}", valueBindingExpression.getBaseExpression().getExpressionString());
        Assert.assertEquals("property2", valueBindingExpression.getProperty());

        valueBindingExpression = new ValueBindingExpression("#{bean1['property1']}");

        valueBindingExpression = ValueBindingExpression.replaceProperty(valueBindingExpression, "property2");

        //TODO restore original syntax
        Assert.assertEquals("#{bean1.property2}", valueBindingExpression.getExpressionString());
        Assert.assertEquals("#{bean1}", valueBindingExpression.getBaseExpression().getExpressionString());
        Assert.assertEquals("property2", valueBindingExpression.getProperty());

        valueBindingExpression = new ValueBindingExpression("#{bean1['bean2'].property1}");

        valueBindingExpression = ValueBindingExpression.replaceProperty(valueBindingExpression, "property2");

        Assert.assertEquals("#{bean1['bean2'].property2}", valueBindingExpression.getExpressionString());
        Assert.assertEquals("#{bean1['bean2']}", valueBindingExpression.getBaseExpression().getExpressionString());
        Assert.assertEquals("#{bean1}", valueBindingExpression.getBaseExpression().getBaseExpression().getExpressionString());
        Assert.assertEquals("property2", valueBindingExpression.getProperty());
    }

    @Test
    public void testStandardSyntaxAddProperty() throws Exception
    {
        ValueBindingExpression valueBindingExpression = new ValueBindingExpression("#{bean1.bean2}");

        valueBindingExpression = ValueBindingExpression.addProperty(valueBindingExpression, "property1");

        Assert.assertEquals("#{bean1.bean2.property1}", valueBindingExpression.getExpressionString());
        Assert.assertEquals("#{bean1.bean2}", valueBindingExpression.getBaseExpression().getExpressionString());
        Assert.assertEquals("property1", valueBindingExpression.getProperty());

        valueBindingExpression = new ValueBindingExpression("#{bean1['bean2']}");

        valueBindingExpression = ValueBindingExpression.addProperty(valueBindingExpression, "property1");

        Assert.assertEquals("#{bean1['bean2'].property1}", valueBindingExpression.getExpressionString());
        Assert.assertEquals("#{bean1['bean2']}", valueBindingExpression.getBaseExpression().getExpressionString());
        Assert.assertEquals("property1", valueBindingExpression.getProperty());

        valueBindingExpression = new ValueBindingExpression("#{bean1['bean2'].bean3}");

        valueBindingExpression = ValueBindingExpression.addProperty(valueBindingExpression, "property1");

        Assert.assertEquals("#{bean1['bean2'].bean3.property1}", valueBindingExpression.getExpressionString());
        Assert.assertEquals("#{bean1['bean2'].bean3}", valueBindingExpression.getBaseExpression().getExpressionString());
        Assert.assertEquals("#{bean1['bean2']}",
                valueBindingExpression.getBaseExpression().getBaseExpression().getExpressionString());
        Assert.assertEquals("property1", valueBindingExpression.getProperty());
    }

    @Test
    public void testFaceletsCustomComponentSyntax() throws Exception
    {
        ValueBindingExpression valueBindingExpression = new ValueBindingExpression("#{entity[fieldName]}");

        Assert.assertEquals("#{entity[fieldName]}", valueBindingExpression.getExpressionString());
        Assert.assertEquals("#{entity}", valueBindingExpression.getBaseExpression().getExpressionString());
        Assert.assertEquals("fieldName", valueBindingExpression.getProperty());
    }

    @Test
    public void testFaceletsCustomComponentSyntaxReplaceProperty() throws Exception
    {
        ValueBindingExpression valueBindingExpression = new ValueBindingExpression("#{entity[fieldName]}");

        valueBindingExpression = ValueBindingExpression.replaceProperty(valueBindingExpression, "newFieldName");

        //TODO restore original syntax
        Assert.assertEquals("#{entity.newFieldName}", valueBindingExpression.getExpressionString());
        Assert.assertEquals("#{entity}", valueBindingExpression.getBaseExpression().getExpressionString());
        Assert.assertEquals("newFieldName", valueBindingExpression.getProperty());
    }

    @Test
    public void testComplexMapSyntax() throws Exception
    {
        ValueBindingExpression valueBindingExpression
                = new ValueBindingExpression("#{bean1[bean2[bean3['key1']]].property1}");

        //TODO
        //assertEquals("#{bean1[bean2[bean3['key1']]].property1}", valueBindingExpression.getExpressionString());
        Assert.assertEquals("property1", valueBindingExpression.getProperty());
    }
}