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

import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;
import org.apache.myfaces.extensions.validator.core.factory.NameMapperAwareFactory;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.InvocationOrder;
import org.apache.myfaces.extensions.validator.core.validation.message.resolver.mapper.*;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.test.core.AbstractExValCoreTestCase;
import org.apache.myfaces.extensions.validator.test.base.mock.MockMessageResolverFactory;
import junit.framework.Test;
import junit.framework.TestSuite;
import junit.framework.Assert;

import java.util.List;

public class RegistrationValidationStrategyToMessageResolverNameMapperTestCase extends AbstractExValCoreTestCase
{
    /**
     * Construct a new instance of the test.
     *
     * @param name Name of the test.
     */
    public RegistrationValidationStrategyToMessageResolverNameMapperTestCase(String name)
    {
        super(name);
    }

    public static Test suite()
    {
        return new TestSuite(RegistrationValidationStrategyToMessageResolverNameMapperTestCase.class);
    }

    public void testValidationStrategyToMessageResolverNameMapperInitialization()
    {
        ExtValUtils.registerValidationStrategyToMessageResolverNameMapper(
                new TestMessageResolverNameMapper());
        ExtValUtils.registerValidationStrategyToMessageResolverNameMapper(
                new TestMessageResolverNameMapper301());
        ExtValUtils.registerValidationStrategyToMessageResolverNameMapper(
                new TestMessageResolverNameMapper150());
        ExtValUtils.registerValidationStrategyToMessageResolverNameMapper(
                new TestMessageResolverNameMapper450());
        ExtValUtils.registerValidationStrategyToMessageResolverNameMapper(
                new TestMessageResolverNameMapper250());
        ExtValUtils.registerValidationStrategyToMessageResolverNameMapper(
                new TestMessageResolverNameMapper550());
        ExtValUtils.registerValidationStrategyToMessageResolverNameMapper(
                new TestMessageResolverNameMapper99());
        ExtValUtils.registerValidationStrategyToMessageResolverNameMapper(
                new TestMessageResolverNameMapper350());

        List<NameMapper<ValidationStrategy>> result = getNameMappers();
        int resultLength = 13;
        Assert.assertEquals(resultLength, result.size());

        for(int i = 0; i < resultLength; i++)
        {
            switch (i)
            {
                case 0:
                    Assert.assertEquals(TestMessageResolverNameMapper99.class, result.get(i).getClass());
                    break;
                case 1:
                    Assert.assertEquals(CustomConfiguredValidationStrategyToMsgResolverNameMapper.class, result.get(i).getClass());
                    break;
                case 2:
                    Assert.assertEquals(TestMessageResolverNameMapper150.class, result.get(i).getClass());
                    break;
                case 3:
                    Assert.assertEquals(CustomConventionValidationStrategyToMsgResolverNameMapper.class, result.get(i).getClass());
                    break;
                case 4:
                    Assert.assertEquals(TestMessageResolverNameMapper250.class, result.get(i).getClass());
                    break;
                case 5:
                    Assert.assertEquals(DefaultValidationStrategyToMsgResolverNameMapper.class, result.get(i).getClass());
                    break;
                case 6:
                    Assert.assertEquals(TestMessageResolverNameMapper301.class, result.get(i).getClass());
                    break;
                case 7:
                    Assert.assertEquals(DefaultModuleValidationStrategyToMsgResolverNameMapper.class, result.get(i).getClass());
                    break;
                case 8:
                    Assert.assertEquals(TestMessageResolverNameMapper350.class, result.get(i).getClass());
                    break;
                case 9:
                    Assert.assertEquals(SimpleValidationStrategyToMsgResolverNameMapper.class, result.get(i).getClass());
                    break;
                case 10:
                    Assert.assertEquals(TestMessageResolverNameMapper450.class, result.get(i).getClass());
                    break;
                case 11:
                    Assert.assertEquals(TestMessageResolverNameMapper550.class, result.get(i).getClass());
                    break;
                case 12:
                    Assert.assertEquals(TestMessageResolverNameMapper.class, result.get(i).getClass());
                    break;
            }
        }
    }

    private List<NameMapper<ValidationStrategy>> getNameMappers()
    {
        NameMapperAwareFactory result = ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.MESSAGE_RESOLVER_FACTORY, NameMapperAwareFactory.class);

        return ((MockMessageResolverFactory)result).getRegisteredNameMapperList();
    }

    class TestMessageResolverNameMapper extends AbstractValidationStrategyToMsgResolverNameMapper
    {
        public String createName(ValidationStrategy source)
        {
            return null;
        }
    }

    @InvocationOrder(99)
    class TestMessageResolverNameMapper99 extends AbstractValidationStrategyToMsgResolverNameMapper
    {
        public String createName(ValidationStrategy source)
        {
            return null;
        }
    }

    @InvocationOrder(150)
    class TestMessageResolverNameMapper150 extends AbstractValidationStrategyToMsgResolverNameMapper
    {
        public String createName(ValidationStrategy source)
        {
            return null;
        }
    }

    @InvocationOrder(250)
    class TestMessageResolverNameMapper250 extends AbstractValidationStrategyToMsgResolverNameMapper
    {
        public String createName(ValidationStrategy source)
        {
            return null;
        }
    }

    @InvocationOrder(301)
    class TestMessageResolverNameMapper301 extends AbstractValidationStrategyToMsgResolverNameMapper
    {
        public String createName(ValidationStrategy source)
        {
            return null;
        }
    }

    @InvocationOrder(350)
    class TestMessageResolverNameMapper350 extends AbstractValidationStrategyToMsgResolverNameMapper
    {
        public String createName(ValidationStrategy source)
        {
            return null;
        }
    }

    @InvocationOrder(450)
    class TestMessageResolverNameMapper450 extends AbstractValidationStrategyToMsgResolverNameMapper
    {
        public String createName(ValidationStrategy source)
        {
            return null;
        }
    }

    @InvocationOrder(550)
    class TestMessageResolverNameMapper550 extends AbstractValidationStrategyToMsgResolverNameMapper
    {
        public String createName(ValidationStrategy source)
        {
            return null;
        }
    }
}