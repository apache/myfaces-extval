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

import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.InvocationOrder;
import org.apache.myfaces.extensions.validator.core.property.PropertyInformation;
import org.apache.myfaces.extensions.validator.core.interceptor.MetaDataExtractionInterceptor;
import org.apache.myfaces.extensions.validator.test.core.AbstractExValCoreTestCase;
import org.junit.Assert;
import org.junit.Test;

import java.util.List;

public class RegistrationMetaDataExtractionInterceptorTestCase extends AbstractExValCoreTestCase
{
    @Test
    public void testMetaDataExtractionInterceptorInitialization()
    {
        ExtValContext.getContext().addMetaDataExtractionInterceptor(new TestMetaDataExtractionInterceptor1000());
        ExtValContext.getContext().addMetaDataExtractionInterceptor(new TestMetaDataExtractionInterceptor2());
        ExtValContext.getContext().addMetaDataExtractionInterceptor(new TestComponentInitializer());
        ExtValContext.getContext().addMetaDataExtractionInterceptor(new TestMetaDataExtractionInterceptor3());
        ExtValContext.getContext().addMetaDataExtractionInterceptor(new TestMetaDataExtractionInterceptor1());

        List<MetaDataExtractionInterceptor> result = ExtValContext.getContext().getMetaDataExtractionInterceptors();

        int resultLength = 5;
        Assert.assertEquals(resultLength, result.size());

        for(int i = 0; i < resultLength; i++)
        {
            switch (i)
            {
                case 0:
                    Assert.assertEquals(TestMetaDataExtractionInterceptor1.class, result.get(i).getClass());
                    break;
                case 1:
                    Assert.assertEquals(TestMetaDataExtractionInterceptor2.class, result.get(i).getClass());
                    break;
                case 2:
                    Assert.assertEquals(TestMetaDataExtractionInterceptor3.class, result.get(i).getClass());
                    break;
                case 3:
                    Assert.assertEquals(TestMetaDataExtractionInterceptor1000.class, result.get(i).getClass());
                    break;
                case 4:
                    Assert.assertEquals(TestComponentInitializer.class, result.get(i).getClass());
                    break;
            }
        }
    }

    class TestComponentInitializer implements MetaDataExtractionInterceptor
    {
        public void afterExtracting(PropertyInformation propertyInformation)
        {
        }
    }

    @InvocationOrder(1)
    class TestMetaDataExtractionInterceptor1 implements MetaDataExtractionInterceptor
    {
        public void afterExtracting(PropertyInformation propertyInformation)
        {
        }
    }

    @InvocationOrder(2)
    class TestMetaDataExtractionInterceptor2 implements MetaDataExtractionInterceptor
    {
        public void afterExtracting(PropertyInformation propertyInformation)
        {
        }
    }

    @InvocationOrder(3)
    class TestMetaDataExtractionInterceptor3 implements MetaDataExtractionInterceptor
    {
        public void afterExtracting(PropertyInformation propertyInformation)
        {
        }
    }

    @InvocationOrder(1000)
    class TestMetaDataExtractionInterceptor1000 implements MetaDataExtractionInterceptor
    {
        public void afterExtracting(PropertyInformation propertyInformation)
        {
        }
    }
}