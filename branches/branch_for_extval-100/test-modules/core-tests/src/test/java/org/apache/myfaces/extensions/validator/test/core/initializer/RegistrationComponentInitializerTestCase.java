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
package org.apache.myfaces.extensions.validator.test.core.initializer;

import junit.framework.Assert;
import junit.framework.Test;
import junit.framework.TestSuite;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.InvocationOrder;
import org.apache.myfaces.extensions.validator.core.initializer.component.ComponentInitializer;
import org.apache.myfaces.extensions.validator.test.core.AbstractExValCoreTestCase;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import java.util.List;
import java.util.Map;

public class RegistrationComponentInitializerTestCase extends AbstractExValCoreTestCase
{
    /**
     * Construct a new instance of the test.
     *
     * @param name Name of the test.
     */
    public RegistrationComponentInitializerTestCase(String name)
    {
        super(name);
    }

    public static Test suite()
    {
        return new TestSuite(RegistrationComponentInitializerTestCase.class);
    }

    public void testComponentInitializerInitialization()
    {
        ExtValContext.getContext().addComponentInitializer(new TestComponentInitializer1000());
        ExtValContext.getContext().addComponentInitializer(new TestComponentInitializer2());
        ExtValContext.getContext().addComponentInitializer(new TestComponentInitializer());
        ExtValContext.getContext().addComponentInitializer(new TestComponentInitializer3());
        ExtValContext.getContext().addComponentInitializer(new TestComponentInitializer1());

        List<ComponentInitializer> result = ExtValContext.getContext().getComponentInitializers();

        int resultLength = 5;
        Assert.assertTrue(result.size() == resultLength);

        for (int i = 0; i < resultLength; i++)
        {
            switch (i)
            {
                case 0:
                    Assert.assertTrue(result.get(i) instanceof TestComponentInitializer1);
                    break;
                case 1:
                    Assert.assertTrue(result.get(i) instanceof TestComponentInitializer2);
                    break;
                case 2:
                    Assert.assertTrue(result.get(i) instanceof TestComponentInitializer3);
                    break;
                case 3:
                    Assert.assertTrue(result.get(i) instanceof TestComponentInitializer1000);
                    break;
                case 4:
                    Assert.assertTrue(result.get(i) instanceof TestComponentInitializer);
                    break;
            }
        }
    }

    class TestComponentInitializer implements ComponentInitializer
    {
        public void configureComponent(FacesContext facesContext, UIComponent uiComponent, Map<String, Object> metaData)
        {
        }
    }

    @InvocationOrder(1)
    class TestComponentInitializer1 implements ComponentInitializer
    {
        public void configureComponent(FacesContext facesContext, UIComponent uiComponent, Map<String, Object> metaData)
        {
        }
    }

    @InvocationOrder(2)
    class TestComponentInitializer2 implements ComponentInitializer
    {
        public void configureComponent(FacesContext facesContext, UIComponent uiComponent, Map<String, Object> metaData)
        {
        }
    }

    @InvocationOrder(3)
    class TestComponentInitializer3 implements ComponentInitializer
    {
        public void configureComponent(FacesContext facesContext, UIComponent uiComponent, Map<String, Object> metaData)
        {
        }
    }

    @InvocationOrder(1000)
    class TestComponentInitializer1000 implements ComponentInitializer
    {
        public void configureComponent(FacesContext facesContext, UIComponent uiComponent, Map<String, Object> metaData)
        {
        }
    }
}