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
package org.apache.myfaces.extensions.validator.test.core.interceptor;

import org.apache.myfaces.extensions.validator.test.AbstractExValViewControllerTestCase;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.interceptor.RendererInterceptor;
import org.apache.myfaces.extensions.validator.core.interceptor.AbstractRendererInterceptor;
import org.apache.myfaces.extensions.validator.PropertyValidationInterceptor;
import junit.framework.Test;
import junit.framework.TestSuite;

import java.util.List;

public class RegistrationRendererInterceptorTestCase extends AbstractExValViewControllerTestCase
{
    /**
     * Construct a new instance of the test.
     *
     * @param name Name of the test.
     */
    public RegistrationRendererInterceptorTestCase(String name)
    {
        super(name);
    }

    public static Test suite()
    {
        return new TestSuite(RegistrationRendererInterceptorTestCase.class);
    }

    public void testRendererInterceptorInitialization()
    {
        ExtValContext.getContext().registerRendererInterceptor(new TestComponentInitializer());
        ExtValContext.getContext().registerRendererInterceptor(new TestComponentInitializer1());
        ExtValContext.getContext().registerRendererInterceptor(new TestComponentInitializer2());

        List<RendererInterceptor> result = ExtValContext.getContext().getRendererInterceptors();

        int resultLength = 4;
        assertEquals(resultLength, result.size());

        RendererInterceptor tmp;
        for(int i = 0; i < resultLength; i++)
        {
            tmp = result.get(i);
            assertTrue(tmp instanceof TestComponentInitializer || tmp instanceof PropertyValidationInterceptor);
        }
    }

    class TestComponentInitializer extends AbstractRendererInterceptor
    {
    }

    class TestComponentInitializer1 extends TestComponentInitializer
    {
    }

    class TestComponentInitializer2 extends TestComponentInitializer
    {
    }
}
