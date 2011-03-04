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
package org.apache.myfaces.extensions.validator.test.beanval;

import org.junit.Test;
import org.junit.Assert;
import org.apache.myfaces.extensions.validator.util.ExtValUtils;
import org.apache.myfaces.extensions.validator.beanval.metadata.transformer.mapper.SizeNameMapper;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.core.validation.strategy.ValidationStrategy;
import org.apache.myfaces.extensions.validator.core.mapper.NameMapper;
import org.apache.myfaces.extensions.validator.core.mapper.SubMapperAwareNameMapper;
import org.apache.myfaces.extensions.validator.core.factory.FactoryNames;
import org.apache.myfaces.extensions.validator.test.base.mock.MockMetaDataTransformerFactory;

import java.util.List;

public class DeregisterSubNameMapperTestCase extends AbstractBeanValidationTestCase
{
    @Test
    public void testDenyNameMapper()
    {
        int count = getNameMapperAndSubNameMapperCount();

        Assert.assertEquals(7, count);

        ExtValUtils.denyValidationStrategyToMetaDataTransformerNameMapper(SizeNameMapper.class);

        count = getNameMapperAndSubNameMapperCount();
        Assert.assertEquals(6, count);

        //no effect because it's denied
        ExtValUtils.registerValidationStrategyToMetaDataTransformerNameMapper(new SizeNameMapper());

        count = getNameMapperAndSubNameMapperCount();
        Assert.assertEquals(6, count);
    }
    @Test
    public void testDeregisterNameMapper()
    {
        int count = getNameMapperAndSubNameMapperCount();

        Assert.assertEquals(7, count);

        ExtValUtils.deregisterValidationStrategyToMetaDataTransformerNameMapper(SizeNameMapper.class);

        count = getNameMapperAndSubNameMapperCount();
        Assert.assertEquals(6, count);

        ExtValUtils.registerValidationStrategyToMetaDataTransformerNameMapper(new SizeNameMapper());

        count = getNameMapperAndSubNameMapperCount();
        Assert.assertEquals(7, count);
    }

    private int getNameMapperAndSubNameMapperCount()
    {
        MockMetaDataTransformerFactory nameMapperAwareFactory =
                (ExtValContext.getContext()
                .getFactoryFinder()
                .getFactory(FactoryNames.META_DATA_TRANSFORMER_FACTORY, MockMetaDataTransformerFactory.class));

        List<NameMapper<ValidationStrategy>> nameMapperList = nameMapperAwareFactory.getRegisteredNameMapperList();

        int count = 0;

        for(NameMapper<ValidationStrategy> nameMapper : nameMapperList)
        {
            if(!(nameMapper instanceof SubMapperAwareNameMapper))
            {
                count++;
            }
        }

        List<NameMapper<ValidationStrategy>> subNamMappers = nameMapperAwareFactory.getSubNameMapperList();
        count += subNamMappers.size();
        return count;
    }
}
