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
package org.apache.myfaces.extensions.validator.test.core.mapper;

import junit.framework.Assert;
import junit.framework.Test;
import junit.framework.TestSuite;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.InvocationOrder;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.factory.NameMapperAwareFactory;
import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;
import org.apache.myfaces.extensions.validator.core.validation.strategy.mapper.*;
import org.apache.myfaces.extensions.validator.test.base.mock.MockValidationStrategyFactory;
import org.apache.myfaces.extensions.validator.test.core.AbstractExValCoreTestCase;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;

import java.util.List;

public class RegistrationMetaDataToValidationStrategyNameMapperTestCase extends AbstractExValCoreTestCase
{
    /**
     * Construct a new instance of the test.
     *
     * @param name Name of the test.
     */
    public RegistrationMetaDataToValidationStrategyNameMapperTestCase(String name)
    {
        super(name);
    }

    public static Test suite()
    {
        return new TestSuite(RegistrationMetaDataToValidationStrategyNameMapperTestCase.class);
    }

    public void testMetaDataToValidationStrategyNameMapperInitialization()
    {
        ExtValUtils.registerMetaDataToValidationStrategyNameMapper(
                new TestValidationStrategyNameMapper());
        ExtValUtils.registerMetaDataToValidationStrategyNameMapper(
                new TestValidationStrategyNameMapper650());
        ExtValUtils.registerMetaDataToValidationStrategyNameMapper(
                new TestValidationStrategyNameMapper150());
        ExtValUtils.registerMetaDataToValidationStrategyNameMapper(
                new TestValidationStrategyNameMapper450());
        ExtValUtils.registerMetaDataToValidationStrategyNameMapper(
                new TestValidationStrategyNameMapper250());
        ExtValUtils.registerMetaDataToValidationStrategyNameMapper(
                new TestValidationStrategyNameMapper550());
        ExtValUtils.registerMetaDataToValidationStrategyNameMapper(
                new TestValidationStrategyNameMapper99());
        ExtValUtils.registerMetaDataToValidationStrategyNameMapper(
                new TestValidationStrategyNameMapper350());

        List<NameMapper<String>> result = getNameMappers();
        int resultLength = 16;
        Assert.assertEquals(resultLength, result.size());

        for (int i = 0; i < resultLength; i++)
        {
            switch (i)
            {
                case 0:
                    Assert.assertEquals(TestValidationStrategyNameMapper99.class, result.get(i).getClass());
                    break;
                case 1:
                    Assert.assertEquals(CustomConfiguredAnnotationToValidationStrategyNameMapper.class, result.get(i).getClass());
                    break;
                case 2:
                    Assert.assertEquals(TestValidationStrategyNameMapper150.class, result.get(i).getClass());
                    break;
                case 3:
                    Assert.assertEquals(CustomConventionAnnotationToValidationStrategyNameMapper.class, result.get(i).getClass());
                    break;
                case 4:
                    Assert.assertEquals(TestValidationStrategyNameMapper250.class, result.get(i).getClass());
                    break;
                case 5:
                    Assert.assertEquals(DefaultAnnotationToValidationStrategyNameMapper.class, result.get(i).getClass());
                    break;
                case 6:
                    Assert.assertEquals(TestValidationStrategyNameMapper350.class, result.get(i).getClass());
                    break;
                case 7:
                    Assert.assertEquals(SimpleAnnotationToValidationStrategyNameMapper.class, result.get(i).getClass());
                    break;
                case 8:
                    Assert.assertEquals(TestValidationStrategyNameMapper450.class, result.get(i).getClass());
                    break;
                case 9:
                    Assert.assertEquals(AnnotationToValidationStrategyBeanNameMapper.class, result.get(i).getClass());
                    break;
                case 10:
                    Assert.assertEquals(AnnotationToValidationStrategyBeanNameMapper.class, result.get(i).getClass());
                    break;
                case 11:
                    Assert.assertEquals(AnnotationToValidationStrategyBeanNameMapper.class, result.get(i).getClass());
                    break;
                case 12:
                    Assert.assertEquals(AnnotationToValidationStrategyBeanNameMapper.class, result.get(i).getClass());
                    break;
                case 13:
                    Assert.assertEquals(TestValidationStrategyNameMapper550.class, result.get(i).getClass());
                    break;
                case 14:
                    Assert.assertEquals(TestValidationStrategyNameMapper650.class, result.get(i).getClass());
                    break;
                case 15:
                    Assert.assertEquals(TestValidationStrategyNameMapper.class, result.get(i).getClass());
                    break;
            }
        }
    }

    private List<NameMapper<String>> getNameMappers()
    {
        NameMapperAwareFactory result = ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.VALIDATION_STRATEGY_FACTORY, NameMapperAwareFactory.class);

        return ((MockValidationStrategyFactory) result).getRegisteredNameMapperList();
    }

    class TestValidationStrategyNameMapper extends AbstractMetaDataToValidationStrategyNameMapper
    {
        public String createName(String source)
        {
            return null;
        }
    }

    @InvocationOrder(99)
    class TestValidationStrategyNameMapper99 extends AbstractMetaDataToValidationStrategyNameMapper
    {
        public String createName(String source)
        {
            return null;
        }
    }

    @InvocationOrder(150)
    class TestValidationStrategyNameMapper150 extends AbstractMetaDataToValidationStrategyNameMapper
    {
        public String createName(String source)
        {
            return null;
        }
    }

    @InvocationOrder(250)
    class TestValidationStrategyNameMapper250 extends AbstractMetaDataToValidationStrategyNameMapper
    {
        public String createName(String source)
        {
            return null;
        }
    }

    @InvocationOrder(350)
    class TestValidationStrategyNameMapper350 extends AbstractMetaDataToValidationStrategyNameMapper
    {
        public String createName(String source)
        {
            return null;
        }
    }

    @InvocationOrder(450)
    class TestValidationStrategyNameMapper450 extends AbstractMetaDataToValidationStrategyNameMapper
    {
        public String createName(String source)
        {
            return null;
        }
    }

    @InvocationOrder(550)
    class TestValidationStrategyNameMapper550 extends AbstractMetaDataToValidationStrategyNameMapper
    {
        public String createName(String source)
        {
            return null;
        }
    }

    @InvocationOrder(650)
    class TestValidationStrategyNameMapper650 extends AbstractMetaDataToValidationStrategyNameMapper
    {
        public String createName(String source)
        {
            return null;
        }
    }
}