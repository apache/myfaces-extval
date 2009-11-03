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

import org.apache.myfaces.extensions.validator.test.AbstractExValViewControllerTestCase;
import org.apache.myfaces.extensions.validator.test.crossval.MockMetaDataTransformerFactory;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;
import org.apache.myfaces.extensions.validator.core.factory.NameMapperAwareFactory;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.InvocationOrder;
import org.apache.myfaces.extensions.validator.core.metadata.transformer.mapper.*;
import junit.framework.Test;
import junit.framework.TestSuite;

import java.util.List;

public class RegistrationValidationStrategyToMetaDataTransformerNameMapperTestCase extends AbstractExValViewControllerTestCase
{
    /**
     * Construct a new instance of the test.
     *
     * @param name Name of the test.
     */
    public RegistrationValidationStrategyToMetaDataTransformerNameMapperTestCase(String name)
    {
        super(name);
    }

    public static Test suite()
    {
        return new TestSuite(RegistrationValidationStrategyToMetaDataTransformerNameMapperTestCase.class);
    }
    public void testValidationStrategyToMetaDataTransformerMapperInitialization()
    {
        ExtValUtils.registerValidationStrategyToMetaDataTransformerNameMapper(
                new TestMetaDataTransformerNameMapper());
        ExtValUtils.registerValidationStrategyToMetaDataTransformerNameMapper(
                new TestMetaDataTransformerNameMapper150());
        ExtValUtils.registerValidationStrategyToMetaDataTransformerNameMapper(
                new TestMetaDataTransformerNameMapper450());
        ExtValUtils.registerValidationStrategyToMetaDataTransformerNameMapper(
                new TestMetaDataTransformerNameMapper250());
        ExtValUtils.registerValidationStrategyToMetaDataTransformerNameMapper(
                new TestMetaDataTransformerNameMapper550());
        ExtValUtils.registerValidationStrategyToMetaDataTransformerNameMapper(
                new TestMetaDataTransformerNameMapper99());
        ExtValUtils.registerValidationStrategyToMetaDataTransformerNameMapper(
                new TestMetaDataTransformerNameMapper350());

        List<NameMapper<ValidationStrategy>> result = getNameMappers();
        int resultLength = 13;
        assertEquals(resultLength, result.size());

        for(int i = 0; i < resultLength; i++)
        {
            switch (i)
            {
                case 0:
                    assertEquals(TestMetaDataTransformerNameMapper99.class, result.get(i).getClass());
                    break;
                case 1:
                    assertEquals(CustomConfiguredValidationStrategyToMetaDataTransformerNameMapper.class, result.get(i).getClass());
                    break;
                case 2:
                    assertEquals(ValidationStrategyToMetaDataTransformerSubMapperAwareNameMapper.class, result.get(i).getClass());
                    break;
                case 3:
                    assertEquals(TestMetaDataTransformerNameMapper150.class, result.get(i).getClass());
                    break;
                case 4:
                    assertEquals(CustomConventionValidationStrategyToMetaDataTransformerNameMapper.class, result.get(i).getClass());
                    break;
                case 5:
                    assertEquals(TestMetaDataTransformerNameMapper250.class, result.get(i).getClass());
                    break;
                case 6:
                    assertEquals(DefaultValidationStrategyToMetaDataTransformerNameMapper.class, result.get(i).getClass());
                    break;
                case 7:
                    assertEquals(TestMetaDataTransformerNameMapper350.class, result.get(i).getClass());
                    break;
                case 8:
                    assertEquals(SimpleValidationStrategyToMetaDataTransformerNameMapper.class, result.get(i).getClass());
                    break;
                case 9:
                    assertEquals(TestMetaDataTransformerNameMapper450.class, result.get(i).getClass());
                    break;
                case 10:
                    assertEquals(BeanValidationStrategyToMetaDataTransformerNameMapper.class, result.get(i).getClass());
                    break;
                case 11:
                    assertEquals(TestMetaDataTransformerNameMapper550.class, result.get(i).getClass());
                    break;
                case 12:
                    assertEquals(TestMetaDataTransformerNameMapper.class, result.get(i).getClass());
                    break;
            }
        }
    }

    private List<NameMapper<ValidationStrategy>> getNameMappers()
    {
        NameMapperAwareFactory result = ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.META_DATA_TRANSFORMER_FACTORY, NameMapperAwareFactory.class);

        return ((MockMetaDataTransformerFactory)result).getRegisteredNameMapperList();
    }

    class TestMetaDataTransformerNameMapper extends AbstractValidationStrategyToMetaDataTransformerNameMapper
    {
        public String createName(ValidationStrategy source)
        {
            return null;
        }
    }

    @InvocationOrder(99)
    class TestMetaDataTransformerNameMapper99 extends AbstractValidationStrategyToMetaDataTransformerNameMapper
    {
        public String createName(ValidationStrategy source)
        {
            return null;
        }
    }

    @InvocationOrder(150)
    class TestMetaDataTransformerNameMapper150 extends AbstractValidationStrategyToMetaDataTransformerNameMapper
    {
        public String createName(ValidationStrategy source)
        {
            return null;
        }
    }

    @InvocationOrder(250)
    class TestMetaDataTransformerNameMapper250 extends AbstractValidationStrategyToMetaDataTransformerNameMapper
    {
        public String createName(ValidationStrategy source)
        {
            return null;
        }
    }

    @InvocationOrder(350)
    class TestMetaDataTransformerNameMapper350 extends AbstractValidationStrategyToMetaDataTransformerNameMapper
    {
        public String createName(ValidationStrategy source)
        {
            return null;
        }
    }

    @InvocationOrder(450)
    class TestMetaDataTransformerNameMapper450 extends AbstractValidationStrategyToMetaDataTransformerNameMapper
    {
        public String createName(ValidationStrategy source)
        {
            return null;
        }
    }

    @InvocationOrder(550)
    class TestMetaDataTransformerNameMapper550 extends AbstractValidationStrategyToMetaDataTransformerNameMapper
    {
        public String createName(ValidationStrategy source)
        {
            return null;
        }
    }
}
