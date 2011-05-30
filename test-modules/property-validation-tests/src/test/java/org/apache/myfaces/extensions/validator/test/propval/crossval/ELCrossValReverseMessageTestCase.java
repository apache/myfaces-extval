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
package org.apache.myfaces.extensions.validator.test.propval.crossval;

import org.apache.myfaces.extensions.validator.crossval.annotation.Equals;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticConfigurationNames;
import org.apache.myfaces.extensions.validator.core.initializer.configuration.StaticInMemoryConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValContext;
import org.apache.myfaces.extensions.validator.test.propval.crossval.mock.MockEqualsValidationStrategy;
import org.apache.myfaces.extensions.validator.test.base.util.MethodUtils;
import org.junit.Assert;
import org.junit.Test;

/**
 * @author Gerhard Petracek
 */
public class ELCrossValReverseMessageTestCase extends ELCrossValTestCase
{

    @Override
    protected void setUpTestCase()
    {
        super.setUpTestCase();

        StaticInMemoryConfiguration config = new StaticInMemoryConfiguration();

        config.addMapping(Equals.class.getName(), MockEqualsValidationStrategy.class.getName());
        ExtValContext.getContext().addStaticConfiguration(StaticConfigurationNames.META_DATA_TO_VALIDATION_STRATEGY_CONFIG, config);
    }

    @Override
    public void testCrossComponentEqualsValidationCorrect() throws Exception
    {
        //don't retest this test-case
    }

    @Override
    public void testCrossComponentEqualsValidationFailedValidation() throws Exception
    {
        //don't retest this test-case
    }

    @Test
    public void testModelAwareCrossEqualsValidationCorrect() throws Exception
    {
        super.testModelAwareCrossEqualsValidationCorrect();
        Assert.assertFalse(MethodUtils.isMethodCalled(MockEqualsValidationStrategy.class, "reverseMessage"));
    }

    @Test
    public void testModelAwareCrossEqualsValidationFailedValidation() throws Exception
    {
        super.testModelAwareCrossEqualsValidationFailedValidation();
        Assert.assertTrue(MethodUtils.isMethodCalled(MockEqualsValidationStrategy.class, "reverseMessage"));
        //1x getReverseErrorMessageSummary and 1x getReverseErrorMessageDetail
        Assert.assertTrue(MethodUtils.checkMethodCalled(MockEqualsValidationStrategy.class, "reverseMessage", 2));
    }
}