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
package org.apache.myfaces.extensions.validator.test.core.config;

import java.lang.annotation.Annotation;
import java.lang.reflect.Method;

import org.apache.myfaces.extensions.validator.core.DefaultExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.core.ExtValCoreConfiguration;
import org.apache.myfaces.extensions.validator.test.core.config.support.ConstraintSourceAwareBean;
import org.apache.myfaces.extensions.validator.test.core.config.support.CustomIgnoreConstraintSource;
import org.apache.myfaces.extensions.validator.util.ConstraintSourceUtils;
import org.apache.myfaces.extensions.validator.util.ReflectionUtils;
import org.junit.Assert;
import org.junit.Test;

/**
 * 
 * since v4
 *
 */
public class ExtValCoreConfigurationIgnoreConstraintSourceAnnotationTestCase extends ExtValCoreConfigurationTestCase
{

    @Override
    protected ExtValCoreConfiguration getCustomExtValCoreConfiguration()
    {
        if (needCustomConfig())
        {

            return new DefaultExtValCoreConfiguration()
            {
                @Override
                public Class<? extends Annotation> ignoreConstraintSourceAnnotation()
                {
                    return CustomIgnoreConstraintSource.class;
                }
            };
        }
        else
        {
            return null;
        }
    }

    @Test
    public void testIgnoreConstraintSourceAnnotationDefault() throws Exception
    {
        Method method = ReflectionUtils.getMethod(ConstraintSourceUtils.class, "isMappedConstraintSourceIgnored",
                Class.class, String.class);
        Assert.assertNotNull(method);
        Assert.assertTrue((Boolean) ReflectionUtils.invokeMethodOfClass(ConstraintSourceUtils.class, method,
                ConstraintSourceAwareBean.class, "property1"));
        Assert.assertFalse((Boolean) ReflectionUtils.invokeMethodOfClass(ConstraintSourceUtils.class, method,
                ConstraintSourceAwareBean.class, "property2"));
    }

    @Test
    public void testIgnoreConstraintSourceAnnotationCustomConfig() throws Exception
    {
        Method method = ReflectionUtils.getMethod(ConstraintSourceUtils.class, "isMappedConstraintSourceIgnored",
                Class.class, String.class);
        Assert.assertNotNull(method);
        Assert.assertFalse((Boolean) ReflectionUtils.invokeMethodOfClass(ConstraintSourceUtils.class, method,
                ConstraintSourceAwareBean.class, "property1"));
        Assert.assertTrue((Boolean) ReflectionUtils.invokeMethodOfClass(ConstraintSourceUtils.class, method,
                ConstraintSourceAwareBean.class, "property2"));
    }


}
